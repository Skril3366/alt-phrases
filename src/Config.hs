{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Config (Config(..), ServerConfig(..), PgConfig(..)) where

data Config = Config
  { serverConfig :: ServerConfig
  , pgConfig :: PgConfig
  }

data ServerConfig = ServerConfig
  { serverPort :: Int
  }

data PgConfig = PgConfig
  { pgHost :: String
  , pgPort :: Int
  , pgUser :: String
  , pgPassword :: String
  , pgDbname :: String
  }
