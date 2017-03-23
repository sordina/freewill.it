{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp
import Options.Generic
import Data.Maybe
import Control.Concurrent.STM.TVar

import qualified Enhancements as E
import qualified Lib  as L

data Options = Options { port :: Maybe Int }
  deriving (Eq, Ord, Show, Generic)

instance ParseRecord Options

main :: IO ()
main = do
  p  <- fmap (fromMaybe 8080 . port) $ getRecord "freewill.it"
  as <- newTVarIO L.initialAppState
  putStrLn $ "Running on http://localhost:" ++ show p ++ "/"
  run p (E.app as)
