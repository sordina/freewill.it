{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( api
    , server
    , initialAppState
    , API
    ) where

import Servant

import API
import DB.Class
import DB.MemDB (initialAppState, MemDBConnection(..))
import qualified Control.Concurrent.STM.TVar as T

api :: Proxy API
api = Proxy

server :: T.TVar AppState -> Server API
server as = enter (runReaderTNat as) authServer
       :<|> enter (runReaderTNat as) choiceServer

authServer :: ServerT AuthAPI AppHandler
authServer = return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers
  where
  AS _ _ _ mockUsers = initialAppState

choiceServer :: ServerT ChoiceAPI AppHandler
choiceServer = list   db
          :<|> name   db
          :<|> view   db
          :<|> add    db
          :<|> choose db
  where
  db :: MemDBConnection (AppHandler x)
  db = MDBC -- Hard-Coded for now, but passed in by Reader later...
