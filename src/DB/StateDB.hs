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
  , emptyAppState
  , tryMaybe
  , LocalState(..)
  )
  where

import DB.Class
import API
import Data.List
import Servant
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

-- DB.Class Instance
-- Local State Version of Database that cannot persist outside of local scope.

data LocalState m = LS

instance MonadState AppState m => Name (LocalState (m x)) m where
  name LS _userid = nameState

instance (MonadError ServantErr m, MonadState AppState m) => Add (LocalState (m x)) m where
  add LS _userid cid o = addState cid o

instance (MonadError ServantErr m, MonadState AppState m) => View (LocalState (m x)) m where
  view LS _userid cid = viewState cid

instance (MonadError ServantErr m, MonadState AppState m) => Choose (LocalState (m x)) m where
  choose LS _userid cid oid = chooseState cid oid

instance MonadState AppState m => List (LocalState (m x)) m where
  list LS _userid = listState

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

getOptionsByChoiceId :: ChoiceID -> [Option] -> [Option]
getOptionsByChoiceId cid = filter ((== cid) . optionChoiceId)

getDecisionByChoiceId :: ChoiceID -> [Decision] -> Maybe Decision
getDecisionByChoiceId = getThing (Just . decisionChoiceId)

tryMaybe :: MonadError ServantErr m => String -> Maybe a -> m a
tryMaybe _ (Just x) = return x
tryMaybe s Nothing  = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

tryBool :: MonadError ServantErr m => String -> Bool -> m ()
tryBool _ True  = return ()
tryBool s False = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

-- Stateful operations

nameState :: MonadState AppState m => Choice -> m Choice
nameState cdata = do
  as     <- get
  let cs  = choices as
      cid = ChoiceID $ newId cs
      c   = cdata { choiceId = Just cid }
  put $ as { choices = c : cs }
  return c

viewState :: (MonadError ServantErr m, MonadState AppState m) => ChoiceID -> m ChoiceAPIData
viewState cid = do
  as    <- get
  c     <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  let os = getOptionsByChoiceId  cid $ options as
      d  = getDecisionByChoiceId cid $ decisions as
  return $ CAD c os d

addState :: (MonadError ServantErr m, MonadState AppState m) => ChoiceID -> Option -> m Option
addState cid' odata = do
  let cid = optionChoiceId odata
      msg = "choiceId " ++ show cid
         ++ " doesn't match choiceId " ++ show cid'
         ++ "... Try checking the JSON and route."

  -- Verify that choice referenced exists
  -- And that only one ID is referenced
  as     <- get
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  _      <- tryBool msg (cid == cid')

  let exD = getDecisionByChoiceId cid $ decisions as
      os  = options as
      oid = OptionID $ newId os
      o   = odata { optionId = Just oid }

  -- Can't deliberate after choice
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  put $ as { options = o : os }
  return o

chooseState :: (MonadError ServantErr m, MonadState AppState m) => ChoiceID -> OptionID -> m Decision
chooseState cid oid = do
  as     <- get
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  o      <- tryMaybe ("Couldn't find option " ++ show oid) $ getOptionById oid $ options as

  let exD = getDecisionByChoiceId cid $ decisions as
      ds  = decisions as
      did = DecisionID $ newId ds
      d   = Decision cid (Just did) o

  -- Can't rechoose
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  put $ as { decisions = d : ds }
  return d

listState :: MonadState AppState m => m [Choice]
listState = choices <$> get
