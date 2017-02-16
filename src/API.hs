{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module API where

import Data.Aeson
import Data.Aeson.TH
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- type Redirect = (Verb 'GET 301) '[JSON] (Headers '[Header "Location" String] ())

type Redirect = Get '[PlainText] NoContent

type API = "users" :> Get '[JSON] [User] :<|> Redirect
