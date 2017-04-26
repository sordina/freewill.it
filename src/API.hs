{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module API where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (unpack)
import Servant
import Servant.Auth.Server
import Data.Swagger (ToSchema, ToParamSchema)
import GHC.Generics
import Database.PostgreSQL.Simple
import Control.Monad.Except
import Web.Internal.HttpApiData
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField

type M = ExceptT ServantErr IO

newtype UserID     = UserID     Integer deriving (Eq, Show, Generic, FromField, ToField)
newtype ChoiceID   = ChoiceID   Integer deriving (Eq, Show, Generic, FromField, ToField)
newtype OptionID   = OptionID   Integer deriving (Eq, Show, Generic, FromField, ToField)
newtype DecisionID = DecisionID Integer deriving (Eq, Show, Generic, FromField, ToField)

data User = User
  { userId        :: Maybe UserID
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

data Choice = Choice
  { choiceId   :: Maybe ChoiceID
  , choiceName :: String
  } deriving (Eq, Show, Generic)

data Option = Option
  { optionChoiceId :: ChoiceID
  , optionId       :: Maybe OptionID
  , optionName     :: String
  } deriving (Eq, Show, Generic)

data Decision = Decision
  { decisionChoiceId :: ChoiceID
  , decisionId       :: Maybe DecisionID
  , decision         :: Option
  } deriving (Eq, Show, Generic)

data ChoiceAPIData = CAD
  { theChoice   :: Choice
  , theOptions  :: [Option]
  , theDecision :: Maybe Decision
  } deriving (Eq, Show, Generic)

instance ToSchema UserID
instance ToSchema User
instance ToSchema ChoiceID
instance ToSchema Choice
instance ToSchema OptionID
instance ToSchema Option
instance ToSchema DecisionID
instance ToSchema Decision
instance ToSchema ChoiceAPIData

instance FromRow Choice
instance FromRow Option

instance ToParamSchema ChoiceID
instance ToParamSchema OptionID

instance ToJWT   User
instance FromJWT User

data Login = Login { username :: String, password :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login


deriveJSON defaultOptions ''UserID
deriveJSON defaultOptions ''User
deriveJSON defaultOptions ''ChoiceID
deriveJSON defaultOptions ''Choice
deriveJSON defaultOptions ''OptionID
deriveJSON defaultOptions ''Option
deriveJSON defaultOptions ''DecisionID
deriveJSON defaultOptions ''Decision
deriveJSON defaultOptions ''ChoiceAPIData

instance FromHttpApiData UserID where
  parseHeader     h = UserID <$> parseHeaderWithPrefix "UserID " h
  parseQueryParam p = UserID <$> parseQueryParam p

instance FromHttpApiData ChoiceID where
  parseHeader     h = ChoiceID <$> parseHeaderWithPrefix "UserID " h
  parseQueryParam p = ChoiceID <$> parseQueryParam p

data AppState = AS {
    options   :: [Option  ]
  , choices   :: [Choice  ]
  , decisions :: [Decision]
  , users     :: [User    ]
  } deriving (Eq, Show, Generic)

emptyAppState :: AppState
emptyAppState = AS [] [] [] []

-- ReqBody '[JSON, FormUrlEncoded] String -- For reference
--
type AuthAPI = "signup"  :> Post '[JSON] [User]
          :<|> "signin"  :> Post '[JSON] [User]
          :<|> "signout" :> Post '[JSON] [User]
          :<|> "users"   :> Get  '[JSON] [User]

type ChoiceCapture = Capture "choiceId" ChoiceID

type ChoiceAPI = Get     '[JSON] [Choice]
            :<|> ReqBody '[JSON] Choice    :> Post '[JSON] Choice
            :<|> ChoiceCapture             :> Get  '[JSON] ChoiceAPIData
            :<|> ChoiceCapture :> "add"    :> ReqBody '[JSON] Option   :> Post '[JSON] Option
            :<|> ChoiceCapture :> "choose" :> ReqBody '[JSON] OptionID :> Post '[JSON] Decision

type API = AuthAPI
      :<|> ("choices" :> ChoiceAPI)
      -- :<|> ("choices" :> Auth '[JWT] User :> ChoiceAPI)

help :: IO ()
help = putStrLn $ Data.Text.unpack $ layout (Proxy :: Proxy API)
