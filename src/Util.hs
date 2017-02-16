{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Util where

import Control.Monad.Error.Class
import Data.ByteString
import Servant
import GHC.Types

-- redirectToUsers :: Server Redirect
-- redirectToUsers = return $ addHeader "/users" ()


type Redirect (a :: Symbol) = Get '[PlainText] NoContent

redirectTo :: MonadError ServantErr m => ByteString -> m a
redirectTo location = throwError $ err302 { errHeaders = [("Location", location)] }
