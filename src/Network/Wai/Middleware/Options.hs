
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- A middleware to respond to Options requests for a servant app
-- very helpful when trying to deal with pre-flight CORS requests.
--
module Network.Wai.Middleware.Options where

import Servant
import Servant.Foreign
import Network.Wai
import Data.Text
import Network.HTTP.Types.Method

{- listFromAPI :: forall k (lang :: k) ftype api.
   (HasForeign lang ftype api, GenerateList ftype (Foreign ftype api))
=> Proxy lang -> Proxy ftype -> Proxy api -> [Req ftype]

  data Req f
  = Req {_reqUrl        :: Url f,
         _reqMethod     :: Method,
         _reqHeaders    :: [HeaderArg f],
         _reqBody       :: Maybe f,
         _reqReturnType :: Maybe f,
         _reqFuncName   :: FunctionName} -}

provideOptions :: (GenerateList NoContent (Foreign NoContent api), HasForeign NoTypes NoContent api)
               => Proxy api -> (Request -> t -> IO b) -> Request -> t -> IO b
provideOptions apiproxy app req res = do
  print req
  print rmeth
  print pinfo
  print mlist
  if rmeth == "OPTIONS"
     then opts
     else app req res
  where
  rmeth = requestMethod req :: Method
  pinfo = pathInfo      req :: [ Text ]
  mlist = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) apiproxy -- :: [Req NoContent]
  opts  = undefined

{-

constructPath =
Req { _reqUrl        = Url {_path = [Segment {unSegment = Static (PathSegment {unPathSegment = "signup"})}], _queryStr = []},
      _reqMethod     = "POST",
      _reqHeaders    = [],
      _reqBody       = Nothing,
      _reqReturnType = Just NoContent,
      _reqFuncName   = FunctionName {unFunctionName = ["post","signup"]}}

-}
