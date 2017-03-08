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
import Util
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader.Class

api :: Proxy API
api = Proxy

-- Type level construction is not associative... :(
-- Could maybe try that named routes thingo.
-- Is it even on Hackage yet?
-- Eh... Who cares!
--
server :: AppState -> Server API
server as = enter (runReaderTNat as) authServer
       :<|> enter (runReaderTNat as) choiceServer
       :<|> redirectTo

authServer :: ServerT AuthAPI AppHandler
authServer = return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers

choiceServer :: ServerT ChoiceAPI AppHandler
choiceServer = name
          :<|> view
          :<|> add
          :<|> choose

-- Ex: curl -v -L -XPOST -H "Content-Type: application/json" --data '"lol"' http://localhost:8080/name | jq .
name :: MonadIO m => Choice -> m Choice
name c = do
  liftIO $ print c
  return c

view :: (MonadIO m, MonadReader AppState m) => Integer -> m (Choice, [Option], Maybe Decision)
view cid = do
  liftIO $ print cid
  x <- ask
  -- AS {options = [], choices = [], decisions = [], users = []}
  return (mockChoice, options x ++ [mockOption1, mockOption2], Just mockDecision)

add :: Monad m => t -> t1 -> m Option
add _choiceId _body = return mockOption1

choose :: Monad m => t -> t1 -> m Option
choose _choiceId _body = return mockOption2

mockChoice :: Choice
mockChoice = Choice (Just 1) "What size thing should I eat?"

mockOption1 :: Option
mockOption1 = Option (Just 1) "Something bigger than my own head"

mockOption2 :: Option
mockOption2 = Option (Just 2) "Something reasonable"

mockDecision :: Decision
mockDecision = Decision mockOption2

mockUsers :: [User]
mockUsers = [ User (Just 1) "Isaac" "Newton"
            , User (Just 2) "Albert" "Einstein"
            , User (Just 3) "Richard" "Feynman"
            ]

initialAppState :: AppState
initialAppState = AS [mockOption1] [] [] []
