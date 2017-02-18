{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}

module RedirectTo ( RedirectTo ) where

import GHC.TypeLits            (Symbol, KnownSymbol, symbolVal)
import Data.Typeable           (Proxy(..))
import Servant.Server.Internal (passToServer)
import Servant
import GHC.Generics
import Data.Typeable
import Servant.Swagger
import Data.Swagger
import Data.Text
import Data.Function ((&))
import Control.Lens ((.~))
import Data.Monoid ((<>))

data RedirectTo (sym :: Symbol) deriving (Typeable, Generic)

instance (HasSwagger sub, KnownSymbol a) => HasSwagger (RedirectTo a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
              & info.description .~ (Just ("Redirects to " <> pack path) :: Maybe Text)
    where
    path = symbolVal (Proxy :: Proxy a)

instance (KnownSymbol sym, HasServer api context)
      => HasServer (RedirectTo sym :> api) context where

  type ServerT (RedirectTo sym :> api) m = String -> ServerT api m

  route Proxy context subserver = route (Proxy :: Proxy api) context (passToServer subserver mheader)
    where
    mheader _ = symbolVal (Proxy :: Proxy sym)
