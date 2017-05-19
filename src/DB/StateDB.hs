{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.StateDB
  ( listState
  , addState
  , nameState
  , chooseState
  , viewState
  , registerState
  , loginState
  , emptyAppState
  , tryMaybe
  , LocalState(..)
  )
  where

import DB.Class
import Data
import Data.List
import Servant
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

-- DB.Class Instance
-- Local State Version of Database that cannot persist outside of local scope.

data LocalState m = LS

instance MonadState AppState m => Name (LocalState (m x)) m where
  name LS uid = nameState uid

instance (MonadError ServantErr m, MonadState AppState m) => Add (LocalState (m x)) m where
  add LS uid cid o = addState uid cid o

instance (MonadError ServantErr m, MonadState AppState m) => View (LocalState (m x)) m where
  view LS uid cid = viewState uid cid

instance (MonadError ServantErr m, MonadState AppState m) => Choose (LocalState (m x)) m where
  choose LS uid cid oid = chooseState uid cid oid

instance MonadState AppState m => List (LocalState (m x)) m where
  list LS uid = listState uid

instance (MonadError ServantErr m, MonadState AppState m) => Register (LocalState (m x)) m where
  register LS un pw = registerState un pw

instance (MonadError ServantErr m, MonadState AppState m) => Login (LocalState (m x)) m where
  login LS un pw = loginState un pw

instance (MonadState AppState m, MonadError ServantErr m) => Database (LocalState (m x)) m where


-- Pure Implementations

getThing :: Eq a => (x -> Maybe a) -> a -> [x] -> Maybe x
getThing f oid = find ((== Just oid) . f)

newId :: [x] -> UUID
newId = UUID . show . length

getChoiceById :: ChoiceID -> [Choice] -> Maybe Choice
getChoiceById = getThing choiceId

getOptionById :: OptionID -> [Option] -> Maybe Option
getOptionById = getThing optionId

getOptionsByChoiceId :: UserID -> ChoiceID -> [Option] -> [Option]
getOptionsByChoiceId uid cid = filter test
  where
  test x = optionChoiceId x == cid
        && optionUserId   x == Just uid


getDecisionByChoiceId :: UserID -> ChoiceID -> [Decision] -> Maybe Decision
getDecisionByChoiceId uid cid ds = find test ds
  where
  test x = decisionChoiceId x == cid
        && decisionUserId   x == Just uid

tryMaybe :: MonadError ServantErr m => String -> Maybe a -> m a
tryMaybe _ (Just x) = return x
tryMaybe s Nothing  = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

tryBool :: MonadError ServantErr m => String -> Bool -> m ()
tryBool _ True  = return ()
tryBool s False = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

-- Stateful operations

nameState :: MonadState AppState m => UserID -> Choice -> m Choice
nameState uid cdata = do
  as     <- get
  let cs  = choices as
      cid = ChoiceID $ newId cs
      c   = cdata { choiceId = Just cid, choiceUserId = Just uid }
  put $ as { choices = c : cs }
  return c

viewState :: (MonadError ServantErr m, MonadState AppState m) => UserID -> ChoiceID -> m ChoiceAPIData
viewState uid cid = do
  as    <- get
  c     <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  let os = getOptionsByChoiceId uid cid $ options as
      d  = getDecisionByChoiceId uid cid $ decisions as
  return $ CAD c os d

addState :: (MonadError ServantErr m, MonadState AppState m) => UserID -> ChoiceID -> Option -> m Option
addState uid cid' odata = do
  let cid = optionChoiceId odata
      msg = "choiceId " ++ show cid
         ++ " doesn't match choiceId " ++ show cid'
         ++ "... Try checking the JSON and route."

  -- Verify that choice referenced exists
  -- And that only one ID is referenced
  as     <- get
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  _      <- tryBool msg (cid == cid')

  let exD = getDecisionByChoiceId uid cid $ decisions as
      os  = options as
      oid = OptionID $ newId os
      o   = odata { optionId = Just oid, optionUserId = Just uid }

  -- Can't deliberate after choice
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  put $ as { options = o : os }
  return o

chooseState :: (MonadError ServantErr m, MonadState AppState m) => UserID -> ChoiceID -> OptionID -> m Decision
chooseState uid cid oid = do
  as     <- get
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  o      <- tryMaybe ("Couldn't find option " ++ show oid) $ getOptionById oid $ options as

  let exD = getDecisionByChoiceId uid cid $ decisions as
      ds  = decisions as
      did = DecisionID $ newId ds
      d   = Decision cid (Just did) o (Just uid)

  -- Can't rechoose
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  put $ as { decisions = d : ds }
  return d

listState :: MonadState AppState m => UserID -> m [Choice]
listState uid = filter test . choices <$> get
  where
  test x = choiceUserId x == Just uid

registerState :: (MonadError ServantErr m, MonadState AppState m) => String -> String -> m UserID
registerState fn ln = do
  as    <- get
  let us = users as
      u  = find (\x -> userFirstName x == fn && userLastName x == ln) us

  when (isJust u) $ throwError (err401 {errReasonPhrase = "Can't create user"})

  -- TODO: Fix this silly "UUID" nonsense, use a hash or something
  --
  let uid = UserID (UUID (fn ++ "~~" ++ ln))
      n   = User (Just uid) fn ln

  put $ as { users = n : us }

  return uid

loginState :: (MonadError ServantErr m, MonadState AppState m) => String -> String -> m UserID
loginState fn ln = do
  as    <- get
  let us = users as
  u     <- tryMaybe "Couldn't log-in" $ find (\x -> userFirstName x == fn && userLastName x == ln) us

  tryMaybe "No ID for user... Weird." $ userId u
