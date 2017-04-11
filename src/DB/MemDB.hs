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
  name (MDBC a) = name a

instance (MonadIO m, MonadError ServantErr m)
      => DBClass.View (MemDBConnection (m x)) m where
  view :: MemDBConnection (m x) -> ID -> m ChoiceAPIData
  view (MDBC a) = view a

instance (MonadIO m, MonadError ServantErr m)
      => DBClass.Add (MemDBConnection (m x)) m where
  add :: MemDBConnection (m x) -> ID -> Option -> m Option
  add (MDBC a) = add a

instance (MonadIO m, MonadError ServantErr m)
      => DBClass.Choose (MemDBConnection (m x)) m where
  choose :: MemDBConnection (m x) -> ID -> ID -> m Decision
  choose (MDBC a) c o = eitherToError =<< liftIO (T.atomically (runExceptT (choose a c o)))

instance MonadIO m => DBClass.List (MemDBConnection (m x)) m where
  list :: MemDBConnection (m x) -> m [Choice]
  list = doStateOnTVar listState . getConnection

doStateOnTVar :: MonadIO m => State AppState [Choice] -> T.TVar AppState -> m [Choice]
doStateOnTVar s v = liftIO $ T.atomically $ do
  b <- T.readTVar v
  let (a,b') = runState s b
  T.writeTVar v b'
  return a

eitherToError :: MonadError e m => Either e a -> m a
eitherToError r =
  case r of Left  e -> throwError e
            Right x -> return x

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

-- Implementation

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

name :: MonadIO m => T.TVar AppState -> Choice -> m Choice
name ast cdata = liftIO $ T.atomically $ do
  as <- T.readTVar ast
  let cs  = choices as
      cid = newId cs
      c   = cdata { choiceId = Just cid }
  T.writeTVar ast $ as { choices = c : cs }
  return c

view :: (MonadIO m, MonadError ServantErr m)
     => T.TVar AppState -> ID -> m ChoiceAPIData
view ast cid = do
  as  <- liftIO $ T.readTVarIO ast
  c   <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  let os = getOptionsByChoiceId  cid $ options as
      d  = getDecisionByChoiceId cid $ decisions as
  return $ CAD c os d

add :: (MonadIO m, MonadError ServantErr m)
     => T.TVar AppState -> ID -> Option -> m Option
add ast cid' odata = do
  -- Verify that choice referenced exists
  let cid = optionChoiceId odata
      msg = "choiceId " ++ show cid ++ " doesn't match choiceId " ++ show cid' ++ "... Try checking the JSON and route."
  as'    <- liftIO $ T.readTVarIO ast
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as'
  _      <- tryBool msg (cid == cid')

  -- Can't deliberate after choice
  let exD = getDecisionByChoiceId cid $ decisions as'
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  liftIO $ T.atomically $ do
    as <- T.readTVar ast
    let os  = options as
        oid = newId os
        o   = odata { optionId = Just oid }
    T.writeTVar ast $ as { options = o : os }
    return o

choose :: T.TVar AppState -> Integer -> Integer -> ExceptT ServantErr T.STM Decision
choose ast cid oid = do
  as'    <- lift $ T.readTVar ast
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as'
  o      <- tryMaybe ("Couldn't find option " ++ show oid) $ getOptionById oid $ options as'

  -- Can't rechoose
  let exD = getDecisionByChoiceId cid $ decisions as'

  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  lift $ do
    as <- T.readTVar ast
    let ds  = decisions as
        did = newId ds
        d   = Decision cid (Just did) o
    T.writeTVar ast $ as { decisions = d : ds }
    return d

listState :: MonadState AppState m => m [Choice]
listState = choices <$> get
