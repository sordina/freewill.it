{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.MemDB
  ( MemDBConnection(..), initialAppState )
  where

import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM      as T
import qualified DB.Class                    as DBClass

import API
import Data.List
import Servant
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

-- DB.Class Instance

newtype MemDBConnection m = MDBC { getConnection :: T.TVar AppState }

instance MonadIO m => DBClass.Name (MemDBConnection (m x)) m where
  name :: MemDBConnection (m x) -> Choice -> m Choice
  name (MDBC a) c = doStateOnTVar (nameState c) a

instance (MonadIO m, MonadError ServantErr m)
      => DBClass.View (MemDBConnection (m x)) m where
  view :: MemDBConnection (m x) -> ID -> m ChoiceAPIData
  view db cid = runDB db (viewState cid)

instance (MonadIO m, MonadError ServantErr m)
      => DBClass.Add (MemDBConnection (m x)) m where
  add :: MemDBConnection (m x) -> ID -> Option -> m Option
  add db cid o = runDB db (addState cid o)

instance (MonadIO m, MonadError ServantErr m)
      => DBClass.Choose (MemDBConnection (m x)) m where
  choose :: MemDBConnection (m x) -> ID -> ID -> m Decision
  choose db cid oid = runDB db (chooseState cid oid)

instance MonadIO m => DBClass.List (MemDBConnection (m x)) m where
  list :: MemDBConnection (m x) -> m [Choice]
  list = doStateOnTVar listState . getConnection

-- TVar Database Helpers - runDB used on exceptional operations (most)

runDB :: (MonadIO m, MonadError e m) => MemDBConnection t -> ExceptT e (State AppState) b -> m b
runDB (MDBC a) m = eitherToError =<< doStateOnTVar (runExceptT m) a

doStateOnTVar :: MonadIO m => State s a -> T.TVar s -> m a
doStateOnTVar s v = liftIO $ T.atomically $ do
  b <- T.readTVar v
  let (a,b') = runState s b
  T.writeTVar v b'
  return a

eitherToError :: MonadError e m => Either e a -> m a
eitherToError r = case r of Left  e -> throwError e
                            Right x -> return x

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

initialAppState :: AppState
initialAppState = AS [mockOption1, mockOption2] [mockChoice] [mockDecision] mockUsers
  where
  mockChoice :: Choice
  mockChoice = Choice (Just 0) "What size thing should I eat?"

  mockOption1 :: Option
  mockOption1 = Option 0 (Just 1) "Something bigger than my own head"

  mockOption2 :: Option
  mockOption2 = Option 0 (Just 2) "Something reasonable"

  mockDecision :: Decision
  mockDecision = Decision 0 (Just 1) mockOption2

  mockUsers :: [User]
  mockUsers = [ User (Just 0) "Isaac" "Newton"
              , User (Just 1) "Albert" "Einstein"
              , User (Just 2) "Richard" "Feynman"
              ]
