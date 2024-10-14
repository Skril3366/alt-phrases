{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype ID a = ID {unID :: Int}
  deriving (Eq, Show, Ord, Generic)

instance FromJSON (ID a)
instance ToJSON (ID a)

-- Users
data UserId

type UserID = ID UserId

data User = User
  { id :: UserID
  , username :: String
  , passwordHash :: String
  }
  deriving (Show)

-- Phrases

data PhraseId

type PhraseID = ID PhraseId

data Phrase = Phrase
  { id :: PhraseID
  , text :: String
  , errors :: [Error]
  , groupId :: PhraseGroupID
  , authorId :: UserID
  }
  deriving (Show)

data Error = Error
  { word :: String
  , corrected :: String
  }
  deriving (Generic, Show)

instance FromJSON Error

-- Phrase Groups

data PhraseGroupId

type PhraseGroupID = ID PhraseGroupId

data PhraseGroup = PhraseGroup
  { id :: PhraseGroupID
  , groupName :: String
  , groupOwner :: UserID
  }
  deriving (Show)
