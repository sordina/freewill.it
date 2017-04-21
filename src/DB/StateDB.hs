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
  , initialAppState
  , emptyAppState
  , tryMaybe
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
  name LS c = nameState c

instance (MonadError ServantErr m, MonadState AppState m) => Add (LocalState (m x)) m where
  add LS cid o = addState cid o

instance (MonadError ServantErr m, MonadState AppState m) => View (LocalState (m x)) m where
  view LS cid = viewState cid

instance (MonadError ServantErr m, MonadState AppState m) => Choose (LocalState (m x)) m where
  choose LS cid oid = chooseState cid oid

instance MonadState AppState m => List (LocalState (m x)) m where
  list LS = listState

instance (MonadState AppState m, MonadError ServantErr m) => Database (LocalState (m x)) m where


-- Pure Implementations

getThing :: (x -> Maybe ID) -> ID -> [x] -> Maybe x
getThing f oid = find ((== Just oid) . f)

newId :: [x] -> Integer
newId = fromIntegral . length

getChoiceById :: ID -> [Choice] -> Maybe Choice
getChoiceById = getThing choiceId

getOptionById :: ID -> [Option] -> Maybe Option
getOptionById = getThing optionId

getOptionsByChoiceId :: ID -> [Option] -> [Option]
getOptionsByChoiceId cid = filter ((== cid) . optionChoiceId)

getDecisionByChoiceId :: ID -> [Decision] -> Maybe Decision
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
      cid = newId cs
      c   = cdata { choiceId = Just cid }
  put $ as { choices = c : cs }
  return c

viewState :: (MonadError ServantErr m, MonadState AppState m) => Integer -> m ChoiceAPIData
viewState cid = do
  as    <- get
  c     <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  let os = getOptionsByChoiceId  cid $ options as
      d  = getDecisionByChoiceId cid $ decisions as
  return $ CAD c os d

addState :: (MonadError ServantErr m, MonadState AppState m) => ID -> Option -> m Option
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
      oid = newId os
      o   = odata { optionId = Just oid }

  -- Can't deliberate after choice
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  put $ as { options = o : os }
  return o

chooseState :: (MonadError ServantErr m, MonadState AppState m) => ID -> ID -> m Decision
chooseState cid oid = do
  as     <- get
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  o      <- tryMaybe ("Couldn't find option " ++ show oid) $ getOptionById oid $ options as

  let exD = getDecisionByChoiceId cid $ decisions as
      ds  = decisions as
      did = newId ds
      d   = Decision cid (Just did) o

  -- Can't rechoose
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  put $ as { decisions = d : ds }
  return d

listState :: MonadState AppState m => m [Choice]
listState = choices <$> get

-- Mocks

emptyAppState :: AppState
emptyAppState = AS [] [] [] []

initialAppState :: AppState
initialAppState = flip execState emptyAppState $ runExceptT $ makeInitialAppState db
  where
  db = LS :: LocalState (ExceptT ServantErr (State AppState) a)

makeInitialAppState :: (Name db m, Choose db m, Add db m, MonadError ServantErr m) => db -> m ()
makeInitialAppState db = do
  c   <- name db (Choice Nothing "What size thing should I eat?")
  cid <- tryMaybe "Can't find choice" $ choiceId c
  _   <- add db cid (Option cid Nothing "Something bigger than my own head")
  o   <- add db cid (Option cid Nothing "Something reasonable")
  oid <- tryMaybe "Can't find option" $ optionId o
  _   <- choose db cid oid
  return ()
