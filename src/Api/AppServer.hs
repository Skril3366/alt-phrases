{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Api.AppServer (ApiEnv(..)) where

import Prelude hiding (Handler)

import qualified Api.Server
import Config (ServerConfig (serverPort))
import Dal.DAO
import Dal.Model (PhraseToInsert (..), UserToInsert (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (decodeUtf8)
import Model
import Network.Wai.Handler.Warp (run)
import Servant
import Utils.Into (Into (into))

-- Use cases:
-- 1. Пользователь может войти на сервис и получить список всех отправленных
--    фраз с фильтрациями по ещё не согласованным фразам и фразам, автором которых
--    он является
--
-- 2. Пользователь может войти на сервис и отправить новую фразу на согласование
--
-- 3. Пользователь может отметить лучший предложенный вариант фразы, что отметит
--    фразу как согласованную
--
-- 4. Пользователь может зайти добавить альтернативный вариант чужой фразы
--
-- 5. Для каждой фразы отправленной на сервис прогоняется spellcheck и выдаётся
--    список слов, в которых были допущены орфографические ошибки

-- Use cases flow:
-- 1. GET /api/v1/users (maybe before, to be able to filter by users) -> GET /api/v1/phrases?filter=...
-- 2. POST /api/v1/phraseGroup -> POST /api/v1/phrase
-- 3. POST /api/v1/phrase/:id/approve
-- 4. GET /api/v1/phraseGroups -> POST /api/v1/phrase
-- 5. POST /api/v1/phrase (automatically spellchecks)

--------------------------------------------------------------------------------
--                                Utils
--------------------------------------------------------------------------------

type BaseApiV1 b = "api" :> "v1" :> b
type BaseAuthenticatedApiV1 b = BasicAuth "alt-phrases" User :> BaseApiV1 b

data ApiEnv = forall dao. (DAO dao) => ApiEnv
  { dao :: dao
  , server :: ServerConfig
  }

type ApiM = ReaderT ApiEnv Handler

--------------------------------------------------------------------------------
--                                Auth
--------------------------------------------------------------------------------

authCheck :: ApiEnv -> BasicAuthCheck User
authCheck env = BasicAuthCheck check
 where
  check (BasicAuthData username password) = do
    runReaderT (verifyUserCredentials (decodeUtf8 username) (decodeUtf8 password)) env
  verifyUserCredentials username password = do
    ApiEnv dao _ <- ask
    eitherUser <- liftIO $ runExceptT $ findUserByUsername username dao
    case eitherUser of
      Left _ -> return Unauthorized
      Right Nothing -> return Unauthorized
      Right (Just user@(User _ _ dbPassword)) ->
        if password == dbPassword
          then return (Authorized user)
          else return Unauthorized

basicAuthServerContext :: ApiEnv -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext env = authCheck env :. EmptyContext

-- POST /api/v1/auth/register
type RegisterEndpoint = BaseApiV1 ("auth" :> "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] ())

data RegisterRequest = RegisterDTO
  { username :: Text
  , password :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON RegisterRequest

registerUser :: RegisterRequest -> ApiM ()
registerUser (RegisterDTO l p) = do
  ApiEnv dao _ <- ask
  let userToInsert = UserToInsert l p
  newUser <- liftIO $ runExceptT (insertNewUser userToInsert dao)
  case newUser of
    Left _ -> throwError err500
    Right _ -> return ()

--------------------------------------------------------------------------------
--                               Users
--------------------------------------------------------------------------------

data UserResponse = UserDTO
  { userId :: Int
  , username :: Text
  }
  deriving (Show, Eq, Generic)

instance Into User UserResponse where
  into (User userId username _) = UserDTO (unID userId) username

instance ToJSON UserResponse

type GetUsersEndpoint = BaseAuthenticatedApiV1 ("users" :> Get '[JSON] [UserResponse])

getUsers :: User -> ApiM [UserResponse]
getUsers _ = do
  ApiEnv dao _ <- ask
  users <- liftIO $ runExceptT (getAllUsers dao)
  case users of
    Left _ -> throwError err500
    Right users -> return $ map (into :: User -> UserResponse) users

--------------------------------------------------------------------------------
--                                Phrases
--------------------------------------------------------------------------------

-- GET /api/v1/phrases?filter=...
-- TODO: return all phrases submitted by users
-- Filter:
--  - Approved/not approved
--  - Authored by me (maybe should make it generic to specify any author)

-- POST /api/v1/phrase

type PostPhraseEndpoint = BaseAuthenticatedApiV1 ("phrase" :> ReqBody '[JSON] PhraseRequest :> Post '[JSON] ())

data PhraseRequest = PhraseDTO
  { text :: Text
  , groupId :: Int
  , shouldSpellcheck :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON PhraseRequest

postPhrase :: User -> PhraseRequest -> ApiM ()
postPhrase (User (ID uId) _ _) (PhraseDTO t gId _) = do
  ApiEnv dao _ <- ask
  -- TODO: spellcheck
  let phrase = PhraseToInsert t [] gId uId
  newPhrase <- liftIO $ runExceptT (insertPhrase phrase dao)
  case newPhrase of
    Left _ -> throwError err500
    Right _ -> return ()

-- POST /api/v1/phrase/:id/approve
-- TODO: approves given phrase, several phrases may be approved in a single
-- group (this is logical as there may be several equivalents)

--------------------------------------------------------------------------------
--                             Phrase Groups
--------------------------------------------------------------------------------

-- POST /api/v1/phraseGroup
-- TODO: create new phrase group

-- GET /api/v1/phraseGroups
-- TODO: return all phrase groups

--------------------------------------------------------------------------------
--                                AppApi
--------------------------------------------------------------------------------

type AppAPI =
  RegisterEndpoint
    :<|> GetUsersEndpoint
    :<|> PostPhraseEndpoint

appLogicT :: ServerT AppAPI ApiM
appLogicT = registerUser :<|> getUsers :<|> postPhrase

readerToHandler :: ApiEnv -> ApiM a -> Handler a
readerToHandler env r = runReaderT r env

appLogic :: ApiEnv -> (RegisterRequest -> Handler ()) :<|> ((User -> Handler [UserResponse]) :<|> (User -> PhraseRequest -> Handler ()))
appLogic env = hoistServerWithContext (Proxy @AppAPI) (Proxy @'[BasicAuthCheck User]) (readerToHandler env) appLogicT

appServer :: ApiEnv -> Application
appServer env = serveWithContext (Proxy @AppAPI) (basicAuthServerContext env) (appLogic env)

--------------------------------------------------------------------------------
--                                Server
--------------------------------------------------------------------------------

instance Api.Server.Server ApiEnv where
  runServer conf = run (serverPort (server conf)) (appServer conf)
