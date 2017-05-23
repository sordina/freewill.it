
{-# LANGUAGE FlexibleContexts #-}

module DB.MockDB
  ( initialAppState
  , makeInitialAppState
  )
  where

import Data
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

makeInitialAppState :: (Database db m, MonadError ServantErr m) => db -> m ()
makeInitialAppState db = do
  u   <- userId <$> register db "hello" (Password "world")
  c   <- name db u (Choice Nothing "What size thing should I eat?" (Just u))
  cid <- tryMaybe "Can't find choice" $ choiceId c
  _   <- add db u cid (Option cid Nothing "Something bigger than my own head" (Just u))
  o   <- add db u cid (Option cid Nothing "Something reasonable" (Just u))
  oid <- tryMaybe "Can't find option" $ optionId o
  _   <- choose db u cid oid
  return ()
