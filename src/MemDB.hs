{-# LANGUAGE FlexibleContexts #-}

module MemDB where

import API
import Data.List
import Servant
import qualified Control.Concurrent.STM.TVar as T
import qualified Control.Concurrent.STM as T
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader.Class
import Control.Monad.Except

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

getDecisionById :: ID -> [Decision] -> Maybe Decision
getDecisionById = getThing decisionId

getDecisionByChoiceId :: ID -> [Decision] -> Maybe Decision
getDecisionByChoiceId = getThing (Just . decisionChoiceId)

tryMaybe :: MonadError ServantErr m => String -> Maybe a -> m a
tryMaybe _ (Just x) = return x
tryMaybe s Nothing  = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

tryBool :: MonadError ServantErr m => String -> Bool -> m ()
tryBool _ True  = return ()
tryBool s False = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

view :: (MonadReader (T.TVar AppState) m, MonadIO m, MonadError ServantErr m)
     => ID -> m (Choice, [Option], Maybe Decision)
view cid = do
  ast <- ask
  as  <- liftIO $ T.readTVarIO ast
  c   <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  let os = getOptionsByChoiceId  cid $ options as
      d  = getDecisionByChoiceId cid $ decisions as
  return (c, os, d)

name :: (MonadReader (T.TVar AppState) m, MonadIO m)
     => Choice -> m Choice
name cdata = do
  ast <- ask
  liftIO $ T.atomically $ do
    as <- T.readTVar ast
    let cs  = choices as
        cid = newId cs
        c   = cdata { choiceId = Just cid }
    T.writeTVar ast $ as { choices = c : cs }
    return c

add :: (MonadReader (T.TVar AppState) m, MonadIO m, MonadError ServantErr m)
     => ID -> Option -> m Option
add cid' odata = do
  -- Verify that choice referenced exists
  let cid = optionChoiceId odata
      msg = "choiceId " ++ show cid ++ " doesn't match choiceId " ++ show cid' ++ "... Try checking the JSON and route."
  ast    <- ask
  as'    <- liftIO $ T.readTVarIO ast
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as'
  _      <- tryBool msg (cid == cid')
  liftIO $ T.atomically $ do
    as <- T.readTVar ast
    let os  = options as
        oid = newId os
        o   = odata { optionId = Just oid }
    T.writeTVar ast $ as { options = o : os }
    return o

choose :: (MonadReader (T.TVar AppState) m, MonadIO m, MonadError ServantErr m)
     => ID -> ID -> m Decision
choose cid oid = do
  ast    <- ask
  as'    <- liftIO $ T.readTVarIO ast
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as'
  o      <- tryMaybe ("Couldn't find option " ++ show oid) $ getOptionById oid $ options as'
  liftIO $ T.atomically $ do
    as <- T.readTVar ast
    let ds  = decisions as
        did = newId ds
        d   = Decision cid (Just did) o
    T.writeTVar ast $ as { decisions = d : ds }
    return d

list :: (MonadReader (T.TVar AppState) m, MonadIO m)
     => m [Choice]
list = do
  ast <- ask
  as  <- liftIO $ T.readTVarIO ast
  return $ choices as

-- Mocks

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

initialAppState :: AppState
initialAppState = AS [mockOption1, mockOption2] [mockChoice] [mockDecision] mockUsers
