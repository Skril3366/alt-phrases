{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Api.AppServer where

import qualified Api.Server
import Config (PgConfig (..), ServerConfig (serverPort))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Dal.DAO
import Dal.PgDAO
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
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

--------------------------------------------------------------------------------
--                                Auth
--------------------------------------------------------------------------------

-- TODO: Use bcrypt function for storing passwords
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData u p) =
        if u == "aboba" && p == "1"
          then return (Authorized (User (ID 1) "servant" "1"))
          else return Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

-- POST /api/v1/auth/register
-- TODO: register new user
type RegisterEndpoint = BaseApiV1 ("auth" :> "register" :> ReqBody '[JSON] RegisterDTO :> Post '[JSON] String)

data RegisterDTO = RegisterDTO
  { login :: String
  , password :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON RegisterDTO

registerUser :: RegisterDTO -> Handler String
registerUser _ = return "why are you running???" -- TODO: implement proper registration

--------------------------------------------------------------------------------
--                               Users
--------------------------------------------------------------------------------

-- GET /api/v1/users
-- TODO: return all users

type UsersEndpoint = BaseAuthenticatedApiV1 ("users" :> Get '[JSON] [UserDTO])

data UserDTO = UserDTO
  { userId :: UserID
  , login :: String
  }
  deriving (Show, Eq, Generic)

instance Into User UserDTO where
  into (User userId login _) = UserDTO userId login

instance ToJSON UserDTO

pgConfig :: PgConfig
pgConfig =
  PgConfig
    { pgHost = "localhost"
    , pgPort = 5432
    , pgUser = "user"
    , pgPassword = "password"
    , pgDbname = "user"
    }

users :: User -> Handler [UserDTO]
users _ = do
  users <- liftIO (getAllUsers pgConfig)
  return $ map (into :: User -> UserDTO) users

-- users _ = liftIO (getAllUsers (pgConfig :: PgConfig)) >>= return . map into

--------------------------------------------------------------------------------
--                                Phrases
--------------------------------------------------------------------------------

-- GET /api/v1/phrases?filter=...
-- TODO: return all phrases submitted by users
-- Filter:
--  - Approved/not approved
--  - Authored by me (maybe should make it generic to specify any author)

-- POST /api/v1/phrase
-- TODO: submit new phrase for approval
-- Body:
--  - phrase
--  - groupId
--  - shouldSpellcheck: Boolean

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
    :<|> UsersEndpoint

-- :<|> Phrases
-- :<|> PhraseGroups

appLogic :: Server AppAPI
appLogic = registerUser :<|> users

appServer :: Application
appServer = serveWithContext (Proxy @AppAPI) basicAuthServerContext appLogic

--------------------------------------------------------------------------------
--                                Server
--------------------------------------------------------------------------------

-- data ServerEnv = ServerEnv
--   { config :: ServerConfig
--   -- , pgConfig :: PgConfig
--   }

instance Api.Server.Server ServerConfig where
  runServer conf = run (serverPort conf) appServer
