{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.MemDB
  ( newMemDBConnection
  , MemDBConnection(..)
  , test
  )
  where

import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM      as T

import DB.Class
import DB.StateDB
import Data
import API (M)
import Servant
import Control.Monad.Except
import Control.Monad.State

-- DB.Class Instance

newtype MemDBConnection m = MDBC { getConnection :: T.TVar AppState }

newMemDBConnection :: IO (MemDBConnection m)
newMemDBConnection = MDBC <$> T.newTVarIO emptyAppState

instance MonadIO m => Name (MemDBConnection (m x)) m where
  name (MDBC a) uid c = doStateOnTVar (nameState uid c) a

instance (MonadIO m, MonadError ServantErr m)
      => View (MemDBConnection (m x)) m where
  view db uid cid = runDB db (viewState uid cid)

instance (MonadIO m, MonadError ServantErr m)
      => Share (MemDBConnection (m x)) m where
  share db uid cid = runDB db (shareState uid cid)
  hide  db uid cid = runDB db (hideState  uid cid)

instance (MonadIO m, MonadError ServantErr m)
      => Add (MemDBConnection (m x)) m where
  add db uid cid o = runDB db (addState uid cid o)

instance (MonadIO m, MonadError ServantErr m)
      => Choose (MemDBConnection (m x)) m where
  choose db uid cid oid = runDB db (chooseState uid cid oid)

instance MonadIO m => List (MemDBConnection (m x)) m where
  list db uid = doStateOnTVar (listState uid) $ getConnection db

instance (MonadIO m, MonadError ServantErr m)
      => Login (MemDBConnection (m x)) m where
  login db fn ln = runDB db (loginState fn ln)

instance (MonadIO m, MonadError ServantErr m)
      => Register (MemDBConnection (m x)) m where
  register db fn ln = runDB db (registerState fn ln)

instance (MonadIO m, MonadError ServantErr m)
      => Me (MemDBConnection (m x)) m where
  me db uid = runDB db (meState uid)

instance ( MonadIO m, MonadError ServantErr m ) => Database (MemDBConnection (m x)) m


-- Test

test :: IO (Either ServantErr ())
test = runExceptT $ do
  d <- liftIO $ MDBC <$> T.newTVarIO emptyAppState :: ExceptT ServantErr IO (MemDBConnection (M ()))
  u <- userId <$> register d "hello" (Password "world")
  c <- name d u (Choice Nothing "TestChoice" (Just u) Nothing)
  i <- tryMaybe "Can't find choice" $ choiceId c
  o <- add  d u i (Option i Nothing "TestOption" (Just u))
  r <- list d u
  v <- view d u i
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

-- | eitherToError (Left (GHC.IO.Exception.userError "Foo testing"))
eitherToError :: MonadError e m => Either e a -> m a
eitherToError r = case r of Left  e -> throwError e
                            Right x -> return x
