
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
import Network.Wai.Internal (ResponseReceived(..))
import Data.Maybe
import Data.List (nub)
import Network.HTTP.Types
import qualified Data.ByteString as B

provideOptions :: (GenerateList NoContent (Foreign NoContent api), HasForeign NoTypes NoContent api)
               => Proxy api -> Middleware
provideOptions apiproxy app req cb = do
  if rmeth == "OPTIONS"
     then optional cb prior pinfo mlist
     else prior
  where
  rmeth = requestMethod req :: Method
  pinfo = pathInfo      req :: [ Text ]
  mlist = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) apiproxy
  prior = app req cb

optional :: (Response -> IO ResponseReceived) -> IO ResponseReceived -> [Text] -> [Req NoContent] -> IO ResponseReceived
optional cb prior ts rs = if null methods
                             then prior
                             else optionsResponse cb methods
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

optionsResponse :: (Response -> IO ResponseReceived) -> [Method] -> IO ResponseReceived
optionsResponse cb ms = cb response
  where
  response = buildResponse ms

buildResponse :: [Method] -> Response
buildResponse ms = responseBuilder s h mempty
  where
  s = Status 200 "OK"
  h = [("Allow", m)] -- Allow: OPTIONS, GET, HEAD, POST
  m = B.intercalate ", " ("OPTIONS" : nub ms)
