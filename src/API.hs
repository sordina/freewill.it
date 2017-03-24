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
import Control.Monad.Trans.Reader
import Util
import qualified Control.Concurrent.STM.TVar as T

type ID = Integer

data User = User
  { userId        :: Maybe ID
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

data Choice = Choice
  { choiceId   :: Maybe ID
  , choiceName :: String
  } deriving (Eq, Show, Generic)

data Option = Option
  { optionChoiceId :: ID
  , optionId :: Maybe ID
  , optionName :: String
  } deriving (Eq, Show, Generic)

data Decision = Decision
  { decisionChoiceId :: ID
  , decisionId :: Maybe ID
  , decision :: Option
  } deriving (Eq, Show, Generic)

data ChoiceAPIData = CAD {
  theChoice :: Choice
  , theOptions :: [Option]
  , theDecision :: Maybe Decision
  } deriving (Eq, Show, Generic)

instance ToSchema User
instance ToSchema Choice
instance ToSchema Option
instance ToSchema Decision
instance ToSchema ChoiceAPIData

deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''Choice
deriveJSON defaultOptions ''Option
deriveJSON defaultOptions ''Decision
deriveJSON defaultOptions ''ChoiceAPIData

data AppState = AS {
    options   :: [Option  ]
  , choices   :: [Choice  ]
  , decisions :: [Decision]
  , users     :: [User    ]
  } deriving (Eq, Show, Generic)

type AppHandler = ReaderT (T.TVar AppState) Handler

instance FromFormUrlEncoded User where
  fromFormUrlEncoded inputs =
    User <$> lkup inputs "ID"
         <*> grab inputs "FirstName"
         <*> grab inputs "LastName"

-- ReqBody '[JSON, FormUrlEncoded] String -- For reference
--
type AuthAPI = "signup"  :> Post '[JSON] [User]
          :<|> "signin"  :> Post '[JSON] [User]
          :<|> "signout" :> Post '[JSON] [User]
          :<|> "users"   :> Get  '[JSON] [User]

type ChoiceCapture = Capture "choiceId" ID

type ChoiceAPI = Get     '[JSON] [Choice]
            :<|> ReqBody '[JSON] Choice    :> Post '[JSON] Choice
            :<|> ChoiceCapture             :> Get  '[JSON] ChoiceAPIData
            :<|> ChoiceCapture :> "add"    :> ReqBody '[JSON] Option :> Post '[JSON] Option
            :<|> ChoiceCapture :> "choose" :> ReqBody '[JSON] ID     :> Post '[JSON] Decision

type API = AuthAPI
      :<|> ("choices" :> ChoiceAPI)
      :<|> Redirect "users"

help :: IO ()
help = putStrLn $ Data.Text.unpack $ layout (Proxy :: Proxy API)
