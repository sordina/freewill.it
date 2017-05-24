{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe

import qualified Enhancements                         as E
import qualified API                                  as A
import qualified DB.MemDB                             as MemDB
import qualified DB.PostgresDB                        as PostgresDB
import qualified Servant.JS                           as SJ
import qualified Context                              as C
import qualified Network.Wai                          as W
import qualified Network.Wai.Middleware.RequestLogger as WL
import qualified Network.Wai.Middleware.Debugging     as WL

data Database = Memory | Postgres    deriving (Eq, Show, Read, Generic)
data LogLevel = Prod   | Dev | Debug deriving (Eq, Show, Read, Generic)

data Options = Options { port     :: Maybe Int
                       , database :: Maybe Database <?> "Memory | Postgres (Default)"
                       , jwtKey   :: Maybe FilePath <?> "JWT Key FilePath"
                       , safeAuth :: Maybe Bool     <?> "False | True (Default) - Mandate HTTPS for Auth"
                       , jsURL    :: Maybe Text     <?> "URL that Javascript points to"
                       , logLevel :: Maybe LogLevel <?> "Prod | Dev (Default) | Debug"
                       }
  deriving (Show, Generic)

instance ParseField  Database
instance ParseField  LogLevel
instance ParseRecord Options

main :: IO ()
main = do
  opts     <- getRecord "freewill.ai"
  let oPort = fromMaybe 8080     $             port     opts
      oDB   = fromMaybe Postgres $ unHelpful $ database opts
      oJSU  = fromMaybe ""       $ unHelpful $ jsURL    opts
      oJKO  =                      unHelpful $ jwtKey   opts
      oSAO  =                      unHelpful $ safeAuth opts
      oLog' = fromMaybe Dev      $ unHelpful $ logLevel opts
      oJSO  = SJ.defCommonGeneratorOptions { SJ.urlPrefix = oJSU }
      oLog  = getLogger oLog'
  ctx      <- C.getContext oJKO oSAO

  putStrLn $ "Using " ++ show oLog' ++ " logging middleware"
  putStrLn $ "Using " ++ show oDB ++ " database driver"
  putStrLn $ "Running on http://localhost:" ++ show oPort ++ "/"
  go ctx oPort oJSO oLog oDB

newMemDBConnection :: IO (MemDB.MemDBConnection (A.M a))
newMemDBConnection = MemDB.newMemDBConnection

newPostgresDBConnection :: IO (PostgresDB.PostgresConnection (A.M a))
newPostgresDBConnection = PostgresDB.newPostgresDBConnection

getLogger :: LogLevel -> W.Middleware
getLogger Prod  = WL.logStdout
getLogger Dev   = WL.logStdoutDev
getLogger Debug = WL.debug . WL.logStdoutDev

go :: C.CTX -> Int -> SJ.CommonGeneratorOptions -> W.Middleware -> Database -> IO ()

go c p jso logger Memory = do
  db <- newMemDBConnection
  run p (E.app c db jso logger)

go c p jso logger Postgres = do
  db <- newPostgresDBConnection
  run p (E.app c db jso logger)
