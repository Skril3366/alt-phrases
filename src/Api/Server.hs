module Api.Server where

class Server env where
  runServer :: env -> IO ()
