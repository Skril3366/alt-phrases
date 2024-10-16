{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use <&>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import Api.AppServer (ApiEnv (..))
import Api.Server
import Config
import Dal.DAO ()
import Dal.PgDAO (pgConnect)
import Utils.Into

config :: Config
config =
  Config
    { serverConfig = ServerConfig{serverPort = 8080}
    , pgConfig =
        PgConfig
          { pgHost = "localhost"
          , pgPort = 5432
          , pgUser = "user"
          , pgPassword = "password"
          , pgDbname = "user"
          }
    }

instance Into Config ApiEnv where
  into config =
    ApiEnv
      { server = serverConfig config
      , dao = pgConnect (pgConfig config)
      }

port :: Int
port = serverPort $ serverConfig config

main :: IO ()
main = do
  putStrLn ("Starting server on port: " ++ show port)
  runServer (into config :: ApiEnv)
