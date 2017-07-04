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
import Data.UUID


-- ID Wrappers

newtype UserID       = UserID       UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype ChoiceID     = ChoiceID     UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype OptionID     = OptionID     UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype DecisionID   = DecisionID   UUID   deriving (Eq, Show, Generic, FromField, ToField)
newtype Password     = Password     String deriving (Eq, Show, Generic, FromField, ToField)

-- Query Types

data ChoiceQuery = SQ


-- Domain Data Types

data Choice = Choice
  { choiceId     :: Maybe ChoiceID
  , choiceName   :: String
  , choiceUserId :: Maybe UserID
  , shared       :: Maybe Bool
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
  -- , theUser     :: Maybe User -- How to keep private things private?
  } deriving (Eq, Show, Generic)

data LoginDetails = LoginDetails
  { username :: String
  , password :: Password
  } deriving (Eq, Show, Generic)

data User = User
  { userId     :: UserID
  , userEmail  :: String
  } deriving (Eq, Show, Generic)


-- Instances

instance ToSchema User
instance ToSchema UserID
instance ToSchema ChoiceID
instance ToSchema Choice
instance ToSchema OptionID
instance ToSchema Option
instance ToSchema DecisionID
instance ToSchema Decision
instance ToSchema ChoiceAPIData
instance ToSchema LoginDetails -- TODO: Should this ever be sent back to the client?
instance ToSchema Password

instance FromRow Choice
instance FromRow LoginDetails
instance FromRow Option
instance FromRow Password

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

-- Need manual instances for UUIDs to match with URL Format, or there will be 404s
--
instance ToJSON UUID where
  toJSON u = String (toText u)

instance FromJSON UUID where
  parseJSON (String s) = case fromText s of
                              Just u  -> return u
                              Nothing -> fail "Couldn't decode UUID String"
  parseJSON _          = fail "Expecting JSON String"

prop_uuid_json :: UUID -> Bool
prop_uuid_json u = Success u == fromJSON (toJSON u)

-- Automatic JSON Derivation
--
deriveJSON defaultOptions ''UserID
deriveJSON defaultOptions ''ChoiceID
deriveJSON defaultOptions ''Choice
deriveJSON defaultOptions ''OptionID
deriveJSON defaultOptions ''Option
deriveJSON defaultOptions ''DecisionID
deriveJSON defaultOptions ''Decision
deriveJSON defaultOptions ''ChoiceAPIData
deriveJSON defaultOptions ''LoginDetails
deriveJSON defaultOptions ''Password
deriveJSON defaultOptions ''User
