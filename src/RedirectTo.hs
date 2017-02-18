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

module RedirectTo ( RedirectTo ) where

import GHC.TypeLits            (Symbol, KnownSymbol, symbolVal)
import Data.Typeable           (Proxy(..))
import Servant.Server.Internal (passToServer)
import Servant
import GHC.Generics
import Data.Typeable
import Servant.Swagger

instance (HasSwagger sub) => HasSwagger (RedirectTo a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)

data RedirectTo (sym :: Symbol) deriving (Typeable, Generic)

instance (KnownSymbol sym, HasServer api context)
      => HasServer (RedirectTo sym :> api) context where

  type ServerT (RedirectTo sym :> api) m = String -> ServerT api m

  route Proxy context subserver = route (Proxy :: Proxy api) context (passToServer subserver mheader)
    where
    mheader _ = symbolVal (Proxy :: Proxy sym)
