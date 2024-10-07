{-# LANGUAGE DeriveGeneric #-}

module Dal.Model where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

newtype ID a = ID {unID :: Int}
  deriving (Eq, Show, Ord)

-- Users
data UserId

type UserID = ID UserId

data User = User
  { userId :: UserID
  , username :: String
  , passwordHash :: String
  }
  deriving (Show)

-- Phrases

data PhraseId

type PhraseID = ID PhraseId

data Phrase = Phrase
  { phraseId :: PhraseID
  , text :: String
  , errors :: [Error]
  , groupId :: PhraseGroupID
  , authorId :: UserID
  } deriving (Show)

data Error = Error
  { word :: String
  , corrected :: String
  }
  deriving (Show, Generic)

instance FromJSON Error

-- Phrase Groups

data PhraseGroupId

type PhraseGroupID = ID PhraseGroupId

data PhraseGroup = PhraseGroup
  { phraseGroupId :: PhraseGroupID
  , groupName :: String
  , groupOwner :: Int
  }
