{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Dal.PgDAO where

import Config (PgConfig, pgDbname, pgHost, pgPassword, pgPort, pgUser)
import Dal.DAO ( DAO(..) )
import qualified Dal.Model as PgModel
import qualified Dal.Queries as Queries
import qualified Data.ByteString.Char8 as BS
import qualified Hasql.Connection as C
import qualified Hasql.Session as S
import Hasql.Statement (Statement)
import qualified Model
import Utils.Into ( Into(..) )


instance Into (PgModel.ID a) (Model.ID b) where
  into (PgModel.ID i) = Model.ID i

instance Into PgModel.User Model.User where
  into (PgModel.User userId username passwordHash) = Model.User (into userId) username passwordHash

instance Into PgModel.Error Model.Error where
  into (PgModel.Error word corrected) = Model.Error word corrected

instance Into PgModel.Phrase Model.Phrase where
  into (PgModel.Phrase phraseId text errors groupId authorId) =
    Model.Phrase (into phraseId) text (map into errors) (into groupId) (into authorId)

runSingleQuery :: Config.PgConfig -> Statement a b -> a -> IO b
runSingleQuery settings query arguments = C.acquire connectionString >>= connect >>= runQuery
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
  connect connResult = case connResult of
    Left err -> fail $ "Connection error: " ++ show err
    Right conn -> return conn
  runQuery conn =
    S.run (S.statement arguments query) conn >>= \queryResult ->
      C.release conn >> case queryResult of
        Left err -> fail $ "Query error: " ++ show err
        Right results -> return results

query :: (Into a1 b) => Statement a2 [a1] -> a2 -> Config.PgConfig -> IO [b]
query q args settings = runSingleQuery settings q args >>= return . map into

instance DAO Config.PgConfig where
  getAllUsers = query Queries.selectUsers ()
  getAllPhrases = query Queries.selectPhrases ()
