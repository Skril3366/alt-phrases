module Dal.DAO where

import Model (Phrase, User)

class DAO env where
  getAllUsers :: env -> IO [User]
  getAllPhrases :: env -> IO [Phrase]
