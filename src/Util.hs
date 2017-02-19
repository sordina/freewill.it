{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Monad.Error.Class
import Data.ByteString.Char8 hiding (map)
import Data.Text (Text())
import Control.Arrow (second)
import Servant
import GHC.Types
import Data.Aeson
import RedirectTo

type Redirect (a :: Symbol) = RedirectTo a :> Get '[PlainText] NoContent

redirectTo :: MonadError ServantErr m => String -> m a
redirectTo location = throwError $ err302 { errHeaders = [("Location", pack location)] }

resultToEither :: Result b -> Either String b
resultToEither (Error   e) = Left e
resultToEither (Success r) = Right r

tryJsonFromFormParams :: FromJSON a => [(Text, Text)] -> Either String a
tryJsonFromFormParams = resultToEither . fromJSON . object . map (second String)
