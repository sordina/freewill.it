{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Monad.Error.Class
import Servant
import GHC.Types
import Data.Aeson
import Control.Monad
import Safe
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import RedirectTo

type Redirect (a :: Symbol) = RedirectTo a :> Get '[PlainText] NoContent

redirectTo :: MonadError ServantErr m => String -> m a
redirectTo location = throwError $ err302 { errHeaders = [("Location", C.pack location)] }

resultToEither :: Result b -> Either String b
resultToEither (Error   e) = Left e
resultToEither (Success r) = Right r

grab :: [(T.Text, T.Text)] -> T.Text -> Either String String
grab inputs input_label =
  case (fmap T.unpack . lookup input_label) inputs of
    Nothing -> Left $ "label " ++ T.unpack input_label ++ " not found"
    Just v  -> Right v

lkup :: Read a => [(T.Text, T.Text)] -> T.Text -> Either String a
lkup inputs input_label =
  case (lookup input_label >=> readMay . T.unpack) inputs of
    Nothing -> Left $ "label " ++ T.unpack input_label ++ " not found"
    Just v  -> Right v
