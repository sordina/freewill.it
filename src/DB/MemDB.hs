{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.MemDB
  ( newMemDBConnection
  , MemDBConnection(..)
  , initialAppState
  , test
  )
  where

import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM      as T
import Data.Functor.Identity

import DB.Class
import API
import Data.List
import Servant
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe

-- DB.Class Instance

newtype MemDBConnection m = MDBC { getConnection :: T.TVar AppState }

newMemDBConnection :: IO (MemDBConnection m)
newMemDBConnection = MDBC <$> T.newTVarIO emptyAppState

instance MonadIO m => Name (MemDBConnection (m x)) m where
  name :: MemDBConnection (m x) -> Choice -> m Choice
  name (MDBC a) c = doStateOnTVar (nameState c) a

instance (MonadIO m, MonadError ServantErr m)
      => View (MemDBConnection (m x)) m where
  view :: MemDBConnection (m x) -> ID -> m ChoiceAPIData
  view db cid = runDB db (viewState cid)

instance (MonadIO m, MonadError ServantErr m)
      => Add (MemDBConnection (m x)) m where
  add :: MemDBConnection (m x) -> ID -> Option -> m Option
  add db cid o = runDB db (addState cid o)

instance (MonadIO m, MonadError ServantErr m)
      => Choose (MemDBConnection (m x)) m where
  choose :: MemDBConnection (m x) -> ID -> ID -> m Decision
  choose db cid oid = runDB db (chooseState cid oid)

instance MonadIO m => List (MemDBConnection (m x)) m where
  list :: MemDBConnection (m x) -> m [Choice]
  list = doStateOnTVar listState . getConnection

instance ( MonadIO m, MonadError ServantErr m ) => Database (MemDBConnection (m x)) m where


-- Local State Version of Database that cannot persist outside of local scope

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


-- Test

test :: IO (Either ServantErr ())
test = runExceptT $ do
  d <- liftIO $ MDBC <$> T.newTVarIO emptyAppState :: ExceptT ServantErr IO (MemDBConnection (M ()))
  c <- name d (Choice Nothing "TestChoice")
  i <- tryMaybe "Can't find choice" $ choiceId c
  o <- add  d i (Option i Nothing "TestOption")
  r <- list d
  v <- view d i
  liftIO $ print c
  liftIO $ print o
  liftIO $ print r
  liftIO $ print v

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

emptyAppState :: AppState
emptyAppState = AS [] [] [] []

initialAppState :: AppState
initialAppState = flip execState emptyAppState $ runExceptT $ do
  let db = LS :: LocalState (ExceptT ServantErr (StateT AppState Identity) a)
  c   <- name db (Choice Nothing "What size thing should I eat?")
  cid <- tryMaybe "Can't find choice" $ choiceId c
  _   <- add db cid (Option cid Nothing "Something bigger than my own head")
  o   <- add db cid (Option cid Nothing "Something reasonable")
  oid <- tryMaybe "Can't find option" $ optionId o
  _   <- choose db cid oid
  return ()
