{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( api
    , server
    , API
    ) where

import Servant

import API
import Util

api :: Proxy API
api = Proxy

server :: Server API
server = authServer :<|> choiceServer :<|> redirectTo

authServer :: Server AuthAPI
authServer = return mockUsers
        :<|> return mockUsers
        :<|> return mockUsers

-- Type level construction is not associative... :(
-- Could maybe try that named routes thingo.
-- Is it even on Hackage yet?
-- Eh... Who cares!
--
choiceServer :: Server ChoiceAPI
choiceServer = name
          :<|> view
          :<|> add
          :<|> choose

-- Ex: curl -v -L -XPOST -H "Content-Type: application/json" --data '"lol"' http://localhost:8080/name | jq .
name :: Monad m => t -> m Choice
name _body = return mockChoice

view :: Monad m => t -> m (Choice, [t1])
view _choiceId = return (mockChoice, [])

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

mockUsers :: [User]
mockUsers = [ User (Just 1) "Isaac" "Newton"
            , User (Just 2) "Albert" "Einstein"
            , User (Just 3) "Richard" "Feynman"
            ]
