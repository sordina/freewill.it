
module Network.Wai.Middleware.Debugging where

import Network.Wai as W
import Text.Groom

debug :: W.Middleware
debug wapp req respond = do
  putStrLn ""
  putStrLn "Got Request"
  putStrLn $ unlines $ map ("    " ++) $ lines $ groom
    [ ( "requestMethod"          , show $ W.requestMethod          req )
    , ( "httpVersion"            , show $ W.httpVersion            req )
    , ( "rawPathInfo"            , show $ W.rawPathInfo            req )
    , ( "rawQueryString"         , show $ W.rawQueryString         req )
    , ( "isSecure"               , show $ W.isSecure               req )
    , ( "remoteHost"             , show $ W.remoteHost             req )
    , ( "pathInfo"               , show $ W.pathInfo               req )
    , ( "queryString"            , show $ W.queryString            req )
    , ( "requestBodyLength"      , show $ W.requestBodyLength      req )
    , ( "requestHeaderHost"      , show $ W.requestHeaderHost      req )
    , ( "requestHeaderRange"     , show $ W.requestHeaderRange     req )
    , ( "requestHeaderReferer"   , show $ W.requestHeaderReferer   req )
    , ( "requestHeaderUserAgent" , show $ W.requestHeaderUserAgent req )
    ]
  putStrLn "  With Headers"
  putStrLn $ unlines $ map ("    " ++) $ lines $ groom $ W.requestHeaders req

  wapp req respond

