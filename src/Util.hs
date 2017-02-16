{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Monad.Error.Class
import Data.ByteString
import Servant

-- redirectToUsers :: Server Redirect
-- redirectToUsers = return $ addHeader "/users" ()

redirectTo :: MonadError ServantErr m => ByteString -> m a
redirectTo location = throwError $ err302 { errHeaders = [("Location", location)] }
