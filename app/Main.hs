{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe
import Control.Concurrent.STM.TVar

import qualified Enhancements as E
import qualified Lib          as L

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

go :: Int -> Database -> IO ()

go p Memory = do
  as <- newTVarIO L.initialAppState
  run p (E.app as)

go p Postgres = do
  as <- newTVarIO L.initialAppState
  run p (E.app as)
