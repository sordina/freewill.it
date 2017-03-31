
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
import Data.Text hiding (null, zipWith)
import Network.HTTP.Types.Method
import Data.Maybe

provideOptions :: (GenerateList NoContent (Foreign NoContent api), HasForeign NoTypes NoContent api)
               => Proxy api -> Middleware
provideOptions apiproxy app req res = do
  print req
  print rmeth
  print pinfo
  print mlist
  if rmeth == "OPTIONS"
     then optional prior pinfo mlist
     else prior
  where
  rmeth = requestMethod req :: Method
  pinfo = pathInfo      req :: [ Text ]
  mlist = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) apiproxy
  prior = app req res

optional :: IO ResponseReceived -> [Text] -> [Req NoContent] -> IO ResponseReceived
optional prior ts rs = if null methods
                          then prior
                          else optionsResponse methods
  where
  methods = mapMaybe (getMethod ts) rs

getMethod :: [Text] -> Req NoContent -> Maybe Method
getMethod rs ps = if and $ zipWith matchSegment rs (_path $ _reqUrl ps)
                     then Just (_reqMethod ps)
                     else Nothing

matchSegment :: Text -> Segment NoContent -> Bool
matchSegment _ ( Segment (Cap _) )                              = True
matchSegment a ( Segment (Static (PathSegment b)) ) | a == b    = True
                                                    | otherwise = False

optionsResponse :: [Method] -> IO ResponseReceived
optionsResponse = undefined
