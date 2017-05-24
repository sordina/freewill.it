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
import qualified Servant.JS    as SJ
import qualified Context       as C

data Database = Memory | Postgres
  deriving (Eq, Show, Read, Generic)

data Options = Options { port     :: Maybe Int
                       , database :: Maybe Database <?> "Memory | Postgres (Default)"
                       , jwtKey   :: Maybe FilePath <?> "JWT Key FilePath"
                       , safeAuth :: Maybe Bool     <?> "False | True (Default) - Mandate HTTPS for Auth"
                       , jsURL    :: Maybe Text     <?> "URL that Javascript points to"
                       }
  deriving (Show, Generic)

instance ParseField  Database
instance ParseRecord Options

main :: IO ()
main = do
  opts     <- getRecord "freewill.ai"
  let oPort = fromMaybe 8080     $             port     opts
      oDB   = fromMaybe Postgres $ unHelpful $ database opts
      oJSU  = fromMaybe ""       $ unHelpful $ jsURL    opts
      oJKO  =                      unHelpful $ jwtKey   opts
      oSAO  =                      unHelpful $ safeAuth opts
      oJSO  = SJ.defCommonGeneratorOptions { SJ.urlPrefix = oJSU }
  ctx      <- C.getContext oJKO oSAO

  putStrLn $ "Using " ++ show oDB ++ " database driver"
  putStrLn $ "Running on http://localhost:" ++ show oPort ++ "/"
  go ctx oPort oJSO oDB

newMemDBConnection :: IO (MemDB.MemDBConnection (A.M a))
newMemDBConnection = MemDB.newMemDBConnection

newPostgresDBConnection :: IO (PostgresDB.PostgresConnection (A.M a))
newPostgresDBConnection = PostgresDB.newPostgresDBConnection

go :: C.CTX -> Int -> SJ.CommonGeneratorOptions -> Database -> IO ()

go c p jso Memory = do
  db <- newMemDBConnection
  run p (E.app c db jso)

go c p jso Postgres = do
  db <- newPostgresDBConnection
  run p (E.app c db jso)
