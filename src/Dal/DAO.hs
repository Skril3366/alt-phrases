module Dal.DAO where

import Dal.Model(PhraseToInsert, UserToInsert)
import Model (Phrase, User)

data DatabaseError = ConnectionError String | QueryError String deriving (Show)

toError :: (Show a) => String -> (String -> DatabaseError) -> Either a b -> Either DatabaseError b
toError msg f = mapLeft (\err -> f (msg ++ show err))

type DatabaseResult a = ExceptT DatabaseError IO a

class DAO env where
  getAllUsers :: env -> DatabaseResult [User]
  findUserByUsername :: Text -> env -> DatabaseResult (Maybe User)
  insertNewUser :: UserToInsert -> env -> DatabaseResult (Maybe User)
  insertPhrase :: PhraseToInsert -> env -> DatabaseResult (Maybe Phrase)
