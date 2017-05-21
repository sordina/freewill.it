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

module Data where

import Data.Aeson
import Data.Aeson.TH
import Servant
import Servant.Auth.Server
import Data.Swagger (ToSchema, ToParamSchema)
import GHC.Generics
import Database.PostgreSQL.Simple
import Web.Internal.HttpApiData
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import qualified Data.ByteString.Char8 as BC
import Database.PostgreSQL.Simple.TypeInfo.Static (typoid, uuid)


-- ID Wrappers

newtype UUID       = UUID       String deriving (Eq, Show, Generic, FromHttpApiData)
newtype UserID     = UserID     UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype ChoiceID   = ChoiceID   UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype OptionID   = OptionID   UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype DecisionID = DecisionID UUID   deriving (Eq, Show, Generic, FromField, ToField)


-- Domain Data Types

data User = User
  { userId        :: Maybe UserID
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

data Choice = Choice
  { choiceId     :: Maybe ChoiceID
  , choiceName   :: String
  , choiceUserId :: Maybe UserID
  } deriving (Eq, Show, Generic)

data Option = Option
  { optionChoiceId :: ChoiceID
  , optionId       :: Maybe OptionID
  , optionName     :: String
  , optionUserId   :: Maybe UserID
  } deriving (Eq, Show, Generic)

data Decision = Decision
  { decisionChoiceId :: ChoiceID
  , decisionId       :: Maybe DecisionID
  , decision         :: Option
  , decisionUserId   :: Maybe UserID
  } deriving (Eq, Show, Generic)

data ChoiceAPIData = CAD
  { theChoice   :: Choice
  , theOptions  :: [Option]
  , theDecision :: Maybe Decision
  } deriving (Eq, Show, Generic)

data LoginDetails = LoginDetails
  { username :: String
  , password :: String
  } deriving (Eq, Show, Generic)

data AppState = AS {
    options   :: [ Option ]
  , choices   :: [ Choice ]
  , decisions :: [ Decision ]
  , users     :: [ User ]
  } deriving (Eq, Show, Generic)


-- Instances

instance FromField UUID where
  fromField f mdata =
    if typeOid f /= typoid uuid
      then returnError Incompatible f ""
      else case BC.unpack <$> mdata of
       Nothing  -> returnError UnexpectedNull f ""
       Just dat -> return (UUID dat)

instance ToField UUID where
  toField (UUID s) = Escape (BC.pack s)

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
instance ToSchema LoginDetails

instance FromRow Choice
instance FromRow LoginDetails
instance FromRow Option

instance ToParamSchema UUID
instance ToParamSchema ChoiceID
instance ToParamSchema OptionID

instance ToJWT   UserID
instance FromJWT UserID

instance FromHttpApiData UserID where
  parseHeader     h = UserID <$> parseHeaderWithPrefix "UserID " h
  parseQueryParam p = UserID <$> parseQueryParam p

instance FromHttpApiData ChoiceID where
  parseHeader     h = ChoiceID <$> parseHeaderWithPrefix "UserID " h
  parseQueryParam p = ChoiceID <$> parseQueryParam p


-- JSON Derivation

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
deriveJSON defaultOptions ''LoginDetails
