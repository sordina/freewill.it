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

import DB.Class
import DB.StateDB
import API
import Servant
import Control.Monad.Except
import Control.Monad.State

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
