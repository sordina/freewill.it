{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe
import Servant ( Context( (:.) ) )

import qualified Servant
import qualified Enhancements        as E
import qualified API                 as A
import qualified DB.MemDB            as MemDB
import qualified DB.PostgresDB       as PostgresDB
import qualified Servant.Auth.Server as Auth

data Database = Memory | Postgres
  deriving (Eq, Show, Read, Generic)

data Options = Options { port     :: Maybe Int
                       , database :: Maybe Database <?> "Memory | Postgres (Default)"
                       , jwtKey   :: Maybe String   <?> "JWT Key (Not Currently Used...)"
                       }
  deriving (Show, Generic)

instance ParseField  Database
instance ParseRecord Options

main :: IO ()
main = do
  opts <- getRecord "freewill.ai"
  ctx  <- getContext opts
  let thePort = fromMaybe 8080     $             port     opts
      theDB   = fromMaybe Postgres $ unHelpful $ database opts
  putStrLn $ "Using " ++ show theDB ++ " database driver"
  putStrLn $ "Running on http://localhost:" ++ show thePort ++ "/"
  go ctx thePort theDB

type CTX = Context '[Auth.CookieSettings, Auth.JWTSettings]

getContext :: o -> IO CTX
getContext _o = do
  k <- Auth.generateKey
  putStrLn $ "JWT KEY: " ++ show k -- TODO: Remove!
  return $ cs :. Auth.defaultJWTSettings k :. Servant.EmptyContext
  where
  cs = Auth.defaultCookieSettings { Auth.cookieIsSecure = Auth.NotSecure } -- TODO: Make this optional for dev-mode...

newMemDBConnection :: IO (MemDB.MemDBConnection (A.M a))
newMemDBConnection = MemDB.newMemDBConnection

newPostgresDBConnection :: IO (PostgresDB.PostgresConnection (A.M a))
newPostgresDBConnection = PostgresDB.newPostgresDBConnection

go :: CTX -> Int -> Database -> IO ()

go c p Memory = do
  db <- newMemDBConnection
  run p (E.app c db)

go c p Postgres = do
  db <- newPostgresDBConnection
  run p (E.app c db)
