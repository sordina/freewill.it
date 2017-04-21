
{-# LANGUAGE FlexibleContexts #-}

module DB.MockDB
  ( initialAppState
  , makeInitialAppState
  )
  where

import API
import DB.Class
import DB.StateDB
import Control.Monad.Except
import Control.Monad.State
import Servant (ServantErr)

-- Mocks

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
