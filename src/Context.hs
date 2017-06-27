{-# LANGUAGE DataKinds #-}

module Context
  ( CTX
  , getContext
  )
  where

import qualified Servant
import qualified Data.Aeson
import qualified Servant.Auth.Server  as Auth
import qualified Crypto.JOSE.JWK      as J
import qualified Data.ByteString.Lazy as BL
import qualified System.Directory     as D

import Data.Maybe (fromJust)
import Servant    ( Context((:.)) )

type CTX = Servant.Context '[Auth.CookieSettings, Auth.JWTSettings]

loadOrGenerateAndSaveKey :: Maybe FilePath -> IO J.JWK
loadOrGenerateAndSaveKey Nothing   = Auth.generateKey
loadOrGenerateAndSaveKey (Just fp) = do
  fe <- D.doesFileExist fp
  if fe then fromJust . Data.Aeson.decode <$> BL.readFile fp
        else generateAndSaveKey fp

generateAndSaveKey :: FilePath -> IO J.JWK
generateAndSaveKey fp = do
  k <- Auth.generateKey
  putStrLn $ "Writing JWK to " ++ fp
  BL.writeFile fp (Data.Aeson.encode k)
  return k

getContext :: Maybe FilePath -> Bool -> IO CTX
getContext keyFile safeAuth = do
  k <- loadOrGenerateAndSaveKey keyFile -- (unHelpful (jwtKey o))
  return $ cs :. Auth.defaultJWTSettings k :. Servant.EmptyContext
  where
  cs       = Auth.defaultCookieSettings { Auth.cookieIsSecure = security }
  security | safeAuth  = Auth.Secure
           | otherwise = Auth.NotSecure
