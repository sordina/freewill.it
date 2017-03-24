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
import MemDB
import qualified Control.Concurrent.STM.TVar as T

api :: Proxy API
api = Proxy

-- Type level construction is not associative... :(
-- Could maybe try that named routes thingo.
-- Is it even on Hackage yet?
-- Eh... Who cares!
--
server :: T.TVar AppState -> Server API
server as = enter (runReaderTNat as) authServer
       :<|> enter (runReaderTNat as) choiceServer

authServer :: ServerT AuthAPI AppHandler
authServer = return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers

choiceServer :: ServerT ChoiceAPI AppHandler
choiceServer = list
          :<|> name
          :<|> view
          :<|> add
          :<|> choose
