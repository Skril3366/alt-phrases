{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Dal.Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype ID a = ID {unID :: Int}
  deriving (Eq, Show, Ord)

-- Users
data UserId

type UserID = ID UserId

data User = User
  { userId :: UserID
  , username :: Text
  , passwordHash :: Text
  }
  deriving (Show)

data UserToInsert = UserToInsert
  { username :: Text
  , passwordHash :: Text
  }
  deriving (Show)

-- Phrases

data PhraseId

type PhraseID = ID PhraseId

data Phrase = Phrase
  { phraseId :: PhraseID
  , text :: Text
  , errors :: [Error]
  , groupId :: PhraseGroupID
  , authorId :: UserID
  }
  deriving (Show)

data Error = Error
  { word :: Text
  , corrected :: Text
  }
  deriving (Show, Generic)

instance FromJSON Error

instance ToJSON Error

data PhraseToInsert = PhraseToInsert
  { text :: Text
  , errors :: [Error]
  , groupId :: Int
  , authorId :: Int
  }
  deriving (Show)

-- Phrase Groups

data PhraseGroupId

type PhraseGroupID = ID PhraseGroupId

data PhraseGroup = PhraseGroup
  { phraseGroupId :: PhraseGroupID
  , groupName :: Text
  , groupOwner :: Int
  }
