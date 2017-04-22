{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe

import qualified Enhancements  as E
import qualified API           as A
import qualified DB.MemDB      as MemDB
import qualified DB.PostgresDB as PostgresDB

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
  putStrLn $ "Using " ++ show theDB ++ " database driver"
  putStrLn $ "Running on http://localhost:" ++ show thePort ++ "/"
  go thePort theDB

newMemDBConnection :: IO (MemDB.MemDBConnection (A.M a))
newMemDBConnection = MemDB.newMemDBConnection

newPostgresDBConnection :: IO (PostgresDB.PostgresConnection (A.M a))
newPostgresDBConnection = PostgresDB.newPostgresDBConnection

go :: Int -> Database -> IO ()

go p Memory = do
  as <- newMemDBConnection
  run p (E.app as)

go p Postgres = do
  as <- newPostgresDBConnection
  run p (E.app as)
