{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe
import Servant ( Context( (:.) ) )
import Data.Aeson (encode, decode)
import System.Directory (doesFileExist)

import qualified Servant
import qualified Enhancements         as E
import qualified API                  as A
import qualified DB.MemDB             as MemDB
import qualified DB.PostgresDB        as PostgresDB
import qualified Servant.Auth.Server  as Auth
import qualified Crypto.JOSE.JWK      as J
import qualified Data.ByteString.Lazy as BL

data Database = Memory | Postgres
  deriving (Eq, Show, Read, Generic)

data Options = Options { port     :: Maybe Int
                       , database :: Maybe Database <?> "Memory | Postgres (Default)"
                       , jwtKey   :: Maybe FilePath <?> "JWT Key FilePath"
                       , safeAuth :: Maybe Bool     <?> "False | True (Default) - Mandate HTTPS for Auth"
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

loadOrGenerateAndSaveKey :: Maybe FilePath -> IO J.JWK
loadOrGenerateAndSaveKey Nothing   = Auth.generateKey
loadOrGenerateAndSaveKey (Just fp) = do
  fe <- doesFileExist fp
  if fe then fromJust . decode <$> BL.readFile fp
        else generateAndSaveKey fp

generateAndSaveKey :: FilePath -> IO J.JWK
generateAndSaveKey fp = do
  k <- Auth.generateKey
  putStrLn $ "Writing JWK to " ++ fp
  BL.writeFile fp (encode k)
  return k

getContext :: Options -> IO CTX
getContext o = do
  k <- loadOrGenerateAndSaveKey (unHelpful (jwtKey o))
  return $ cs :. Auth.defaultJWTSettings k :. Servant.EmptyContext
  where
  cs       = Auth.defaultCookieSettings { Auth.cookieIsSecure = security }
  sa       = unHelpful (safeAuth o)
  security | sa == Just False = Auth.NotSecure
           | otherwise        = Auth.Secure

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
