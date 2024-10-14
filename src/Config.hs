module Config where

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
