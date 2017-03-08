{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module API where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (unpack)
import Servant
import Data.Swagger (ToSchema)
import GHC.Generics
import Util

data User = User
  { userId        :: Maybe Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

data Choice = Choice
  { choiceId   :: Maybe Int
  , choiceName :: String
  } deriving (Eq, Show, Generic)

data Option = Option
  { optionId   :: Maybe Int
  , optionName :: String
  } deriving (Eq, Show, Generic)

instance ToSchema User
instance ToSchema Choice
instance ToSchema Option

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Choice)
$(deriveJSON defaultOptions ''Option)

data AppState = AS {
    options :: [Option]
  , choices :: [Choice]
  , users   :: [User]
  } deriving (Eq, Show, Generic)

type ID = Integer

instance FromFormUrlEncoded User where
  fromFormUrlEncoded inputs =
    User <$> lkup inputs "ID"
         <*> grab inputs "FirstName"
         <*> grab inputs "LastName"

-- ReqBody '[JSON, FormUrlEncoded] String -- For reference
--
type AuthAPI = "signup"  :> Post '[JSON] [User] -- TODO
          :<|> "signin"  :> Post '[JSON] [User] -- TODO
          :<|> "signout" :> Post '[JSON] [User] -- TODO

type ChoiceAPI = "choice"  :> ReqBody '[JSON] Choice                                      :> Post '[JSON] Choice
            :<|> "choice"  :> Capture "choiceId" ID :> "view"                             :> Get '[JSON] (Choice, [Option])
            :<|> "choice"  :> Capture "choiceId" ID :> "add"    :> ReqBody '[JSON] Option :> Post '[JSON] Option
            :<|> "choice"  :> Capture "choiceId" ID :> "choose" :> ReqBody '[JSON] ID     :> Post '[JSON] Option

type API = AuthAPI :<|> ChoiceAPI :<|> Redirect "view"

help :: IO ()
help = putStrLn $ Data.Text.unpack $ layout (Proxy :: Proxy API)
