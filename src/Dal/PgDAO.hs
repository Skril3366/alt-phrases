{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dal.PgDAO where

import Config (PgConfig, pgDbname, pgHost, pgPassword, pgPort, pgUser)
import Dal.DAO
import qualified Dal.Model as PgModel
import qualified Dal.Queries as Queries
import qualified Data.ByteString.Char8 as BS
import qualified Hasql.Connection as C
import qualified Hasql.Session as S
import Hasql.Statement (Statement (..))
import qualified Model
import Utils.Into

instance Into (PgModel.ID a) (Model.ID b) where
  into (PgModel.ID i) = Model.ID i

instance Into PgModel.User Model.User where
  into (PgModel.User userId username passwordHash) = Model.User (into userId) username passwordHash

instance Into PgModel.Error Model.Error where
  into (PgModel.Error word corrected) = Model.Error word corrected

instance Into PgModel.Phrase Model.Phrase where
  into (PgModel.Phrase phraseId text errors groupId authorId) =
    Model.Phrase (into phraseId) text (map into errors) (into groupId) (into authorId)

-- NOTE: should add retries on connection errors
pgConnect :: Config.PgConfig -> DatabaseResult C.Connection
pgConnect settings = ExceptT $ fmap (toError "Connection error: " ConnectionError) (C.acquire connectionString)
 where
  connectionString =
    BS.pack $
      unwords $
        map
          (\(k, v) -> k ++ "=" ++ v settings)
          [ ("host", pgHost)
          , ("port", show . pgPort)
          , ("user", pgUser)
          , ("password", pgPassword)
          , ("dbname", pgDbname)
          ]

query :: (Into b c) => Statement a b -> a -> DatabaseResult C.Connection -> DatabaseResult c
query queryStatement args connRes = fmap into runSingleQuery
 where
  runSingleQuery =
    connRes >>= \conn ->
      ExceptT $
        fmap
          (toError "Query error" QueryError)
          (S.run (S.statement args queryStatement) conn)

instance DAO (DatabaseResult C.Connection) where
  getAllUsers = query Queries.selectUsers ()
  insertNewUser = query Queries.insertUser
  findUserByUsername =  query Queries.findUserByUsername
  insertPhrase = query Queries.insertPhrase
