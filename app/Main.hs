{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Main (main) where

import Dal.DAO ()
import Dal.PgDAO ()
import Config
import Api.Server
import Api.AppServer ()

config :: Config
config = Config {
  serverConfig = ServerConfig { serverPort = 8080 },
  pgConfig = PgConfig {
    pgHost = "localhost",
    pgPort = 5432,
    pgUser = "user",
    pgPassword = "password",
    pgDbname = "user"
}}


-- main :: IO ()
-- main = getAllPhrases connection >>= return . show >>= putStrLn

main :: IO ()
main = do
  putStrLn "Starting server..."
  runServer (serverConfig config)
