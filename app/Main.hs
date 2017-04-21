{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe

import qualified Enhancements as E
import qualified DB.MemDB     as DB
import qualified API          as A

data Database = Memory | Postgres
  deriving (Eq, Show, Read, Generic)

data Options = Options { port     :: Maybe Int
                       , database :: Maybe Database <?> "Memory | Postgres"
                       }
  deriving (Show, Generic)

instance ParseField  Database
instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "freewill.it"
  let thePort = fromMaybe 8080     $             port     opts
      theDB   = fromMaybe Postgres $ unHelpful $ database opts
  putStrLn $ "Running on http://localhost:" ++ show thePort ++ "/"
  go thePort theDB

newConnection :: IO (DB.MemDBConnection (A.M a))
newConnection = DB.newMemDBConnection

go :: Int -> Database -> IO ()

go p Memory = do
  as <- newConnection
  run p (E.app as)

go p Postgres = do
  as <- newConnection
  run p (E.app as)
