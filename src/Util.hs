{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Util where

import Control.Monad.Error.Class
import Data.ByteString.Char8
import Servant
import GHC.Types
import RedirectTo

-- import GHC.TypeLits
-- import Data.ByteString.Char8 as C
-- redirectToUsers :: Server Redirect
-- redirectToUsers = return $ addHeader "/users" ()


type Redirect (a :: Symbol) = RedirectTo a :> Get '[PlainText] NoContent

redirectTo :: MonadError ServantErr m => String -> m a
redirectTo location = throwError $ err302 { errHeaders = [("Location", pack location)] }
