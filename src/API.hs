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
import Util

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- type Redirect = (Verb 'GET 301) '[JSON] (Headers '[Header "Location" String] ())

type API = "users"   :> Get  '[JSON] [User]
      :<|> "signup"  :> Post '[JSON] [User]
      :<|> "signin"  :> Post '[JSON] [User]
      :<|> "signout" :> Post '[JSON] [User]
      :<|> "name"    :> ReqBody '[JSON, PlainText] String :> Post '[JSON] [User]
      :<|> "extend"  :> Post '[JSON] [User]
      :<|> "view"    :> Get  '[JSON] [User]
      :<|> "make"    :> Post '[JSON] [User]
      :<|> Redirect "users"
