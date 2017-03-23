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
{-# LANGUAGE FlexibleContexts      #-}

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
import Servant.Foreign.Internal
import Data.Function ((&))
import Control.Lens ((.~))

data RedirectTo (sym :: Symbol) deriving (Typeable, Generic)

setDescription :: String -> Swagger -> Swagger
setDescription s = info.description .~ Just (pack s)

instance (HasSwagger sub, KnownSymbol a) => HasSwagger (RedirectTo a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
              & setDescription ("Redirects to " ++ symbolVal (Proxy :: Proxy a))

-- For implementation
instance (KnownSymbol sym, HasServer api context)
      => HasServer (RedirectTo sym :> api) context where

  type ServerT (RedirectTo sym :> api) m = String -> ServerT api m

  route Proxy context subserver = route (Proxy :: Proxy api) context (passToServer subserver mheader)
    where
    mheader _ = symbolVal (Proxy :: Proxy sym)

-- For js serialization, etc.
instance HasForeign lang ftype api
      => HasForeign lang ftype (RedirectTo sym :> api) where

  type Foreign ftype (RedirectTo sym :> api) = Foreign ftype api

  foreignFor lang ftype Proxy = foreignFor lang ftype (Proxy :: Proxy api)
