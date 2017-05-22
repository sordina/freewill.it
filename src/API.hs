{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Can remove once we've used a package for our instance

module API where

import Data

import Data.Text (unpack)
import Servant
import Servant.Auth.Server
import Servant.Foreign
import Database.PostgreSQL.Simple.FromField
import Data.Swagger.Internal.ParamSchema
import Control.Monad.Except

type M = ExceptT ServantErr IO

type ChoiceCapture = Capture "choiceId" ChoiceID
type LoginHead     = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] UserID
type LoginAPI      = "login"     :>   ReqBody '[JSON] LoginDetails :> Post '[JSON] LoginHead
type RegisterAPI   = "register"  :>   ReqBody '[JSON] LoginDetails :> Post '[JSON] LoginHead
type ChoiceAPI     = Get     '[JSON] [Choice]
                :<|> ReqBody '[JSON] Choice    :> Post '[JSON] Choice
                :<|> ChoiceCapture             :> Get  '[JSON] ChoiceAPIData
                :<|> ChoiceCapture :> "add"    :> ReqBody '[JSON] Option   :> Post '[JSON] Option
                :<|> ChoiceCapture :> "choose" :> ReqBody '[JSON] OptionID :> Post '[JSON] Decision
type AuthAPI       = RegisterAPI
                :<|> LoginAPI
type API           = AuthAPI
                :<|> "me"      :> Auth '[JWT, Cookie] UserID :> Get  '[JSON] UserID
                :<|> "logout"  :> Auth '[JWT, Cookie] UserID :> Post '[JSON] UserID -- TODO: Use empty response somehow
                :<|> "choices" :> Auth '[JWT, Cookie] UserID :> ChoiceAPI

-- Should be provided by a package soon?
-- https://github.com/plow-technologies/servant-auth/issues/8
-- Don't want to muck around with authorization for vanilla js...
instance ( HasForeign lang ftype api , HasForeignType lang ftype 'Text )
    => HasForeign lang ftype (Auth '[JWT, Cookie] a :> api) where

  type Foreign ftype (Auth '[JWT, Cookie] a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) subR -- was req, but that enforces an arg...

instance ToParamSchema SetCookie where
  toParamSchema = mempty

help :: IO ()
help = putStrLn $ Data.Text.unpack $ layoutWithContext (Proxy :: Proxy API) unusedContext
  where
  unusedContext :: Context '[CookieSettings, JWTSettings]
  unusedContext = undefined
