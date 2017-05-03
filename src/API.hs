{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Can remove once we've used a package for our instance

module API where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (unpack)
import Servant
import Servant.Auth.Server
import Servant.Foreign
import Data.Swagger (ToSchema, ToParamSchema)
import GHC.Generics
import Database.PostgreSQL.Simple
import Control.Monad.Except
import Web.Internal.HttpApiData
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString.Char8 as BC
import Database.PostgreSQL.Simple.TypeInfo.Static (typoid, uuid)

type M = ExceptT ServantErr IO

newtype UUID       = UUID       String deriving (Eq, Show, Generic, FromHttpApiData)
newtype UserID     = UserID     UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype ChoiceID   = ChoiceID   UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype OptionID   = OptionID   UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype DecisionID = DecisionID UUID   deriving (Eq, Show, Generic, FromField, ToField)

instance FromField UUID where
  fromField f mdata =
    if typeOid f /= typoid uuid
      then returnError Incompatible f ""
      else case BC.unpack <$> mdata of
       Nothing  -> returnError UnexpectedNull f ""
       Just dat -> return (UUID dat)

instance ToField UUID where
  toField (UUID s) = Escape (BC.pack s)

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

instance ToSchema UUID
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

instance ToParamSchema UUID
instance ToParamSchema ChoiceID
instance ToParamSchema OptionID

data Login = Login { username :: String, password :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

instance ToJWT   UserID
instance FromJWT UserID

deriveJSON defaultOptions ''UUID
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
      :<|> "choices" :> Auth '[JWT] UserID :> ChoiceAPI


-- Should be provided by a package soon?
-- https://github.com/plow-technologies/servant-auth/issues/8
-- Don't want to muck around with authorization for vanilla js...
instance ( HasForeign lang ftype api , HasForeignType lang ftype 'Text )
    => HasForeign lang ftype (Auth '[JWT] a :> api) where

  type Foreign ftype (Auth '[JWT] a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR -- was req, but that enforces an arg...
    {-
    where
      req = subR{ _reqHeaders = HeaderArg arg : _reqHeaders subR }
      arg = Arg
        { _argName = PathSegment "Authorization"
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy 'Text)
        }
      -}

help :: IO ()
help = putStrLn $ Data.Text.unpack $ layoutWithContext (Proxy :: Proxy API) unusedContext
  where
  unusedContext :: Context '[CookieSettings, JWTSettings]
  unusedContext = undefined
