{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Main (main) where

import Dal.DAO
import Dal.PgDAO

connection :: PgConnectionSettings
connection =
  PgConnectionSettings
    { host = "localhost"
    , port = 5432
    , user = "user"
    , password = "password"
    , dbname = "user"
    }

main :: IO ()
main = getAllPhrases connection >>= return . show >>= putStrLn
