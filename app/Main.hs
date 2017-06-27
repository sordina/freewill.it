{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe
import Control.Monad (when)
import Data.List     (isPrefixOf)

import qualified Enhancements                         as E
import qualified API                                  as A
import qualified DB.MemDB                             as MemDB
import qualified DB.PostgresDB                        as PostgresDB
import qualified Servant.JS                           as SJ
import qualified Context                              as C
import qualified Network.Wai                          as W
import qualified Network.Wai.Middleware.RequestLogger as WL
import qualified Network.Wai.Middleware.Debugging     as WL
import qualified Network.Wai.Middleware.ForceSSL      as WS

data LogLevel = Prod   | Dev | Debug deriving (Eq, Show, Read, Generic)

data Options = Options { port     :: Maybe Int
                       , database :: Maybe String   <?> "memory:// (Default) | postgres://... (see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING)"
                       , jwtKey   :: Maybe FilePath <?> "JWT Key FilePath"
                       , safeAuth :: Maybe Bool     <?> "False | True (Default) - Mandate HTTPS for Auth"
                       , jsURL    :: Maybe Text     <?> "URL that Javascript points to"
                       , logLevel :: Maybe LogLevel <?> "Prod | Dev (Default) | Debug"
                       }
  deriving (Show, Generic)

instance ParseField  LogLevel
instance ParseRecord Options

main :: IO ()
main = do
  opts     <- getRecord "freewill.ai"
  let oPort = fromMaybe 8080        $             port     opts
      oDB   = fromMaybe "memory://" $ unHelpful $ database opts
      oJSU  = fromMaybe ""          $ unHelpful $ jsURL    opts
      oJKO  =                         unHelpful $ jwtKey   opts
      oSec  = fromMaybe True        $ unHelpful $ safeAuth opts
      oLog' = fromMaybe Dev         $ unHelpful $ logLevel opts
      oJSO  = SJ.defCommonGeneratorOptions { SJ.urlPrefix = oJSU }
      oLog  = getLogger oLog'
      oSSL  = getHttpsRedirect oSec
  ctx      <- C.getContext oJKO oSec

  putStrLn  $ "Using " ++ show oLog' ++ " logging middleware"
  putStrLn  $ "Using " ++ show oDB   ++ " database driver"
  putStrLn  $ "Running on http://localhost:" ++ show oPort ++ "/"
  when oSec $ putStrLn $ "Mandating SSL"
  go ctx oPort oJSO oLog oSSL oDB

newMemDBConnection :: IO (MemDB.MemDBConnection (A.M a))
newMemDBConnection = MemDB.newMemDBConnection

newPostgresDBConnection :: String -> IO (PostgresDB.PostgresConnection (A.M a))
newPostgresDBConnection dbc = PostgresDB.newPostgresDBConnection dbc

getHttpsRedirect :: Bool -> W.Middleware
getHttpsRedirect False = id
getHttpsRedirect True  = WS.forceSSL

getLogger :: LogLevel -> W.Middleware
getLogger Prod  = WL.logStdout
getLogger Dev   = WL.logStdoutDev
getLogger Debug = WL.debug . WL.logStdoutDev

go :: C.CTX -> Int -> SJ.CommonGeneratorOptions -> W.Middleware -> W.Middleware -> String -> IO ()

go c p jso logger ssl dbc | "memory://" `isPrefixOf` dbc = do
  db <- newMemDBConnection
  run p (logger (ssl (E.app c db jso)))

go c p jso logger ssl dbc | "postgres://" `isPrefixOf` dbc = do
  db <- newPostgresDBConnection dbc
  run p (logger (ssl (E.app c db jso)))

go _c _p _jso _logger _ssl dbc = error ("Invalid database connection string: " ++ dbc)
