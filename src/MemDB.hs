{-# LANGUAGE FlexibleContexts #-}

module MemDB where

import API
import Data.List
import Servant
import qualified Control.Concurrent.STM.TVar as T
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

view :: (MonadReader (T.TVar AppState) m, MonadIO m, MonadError ServantErr m)
     => ID -> m (Choice, [Option], Maybe Decision)
view cid = do
  ast <- ask
  as  <- liftIO $ T.readTVarIO ast
  c   <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  let os = getOptionsByChoiceId  cid $ options as
      d  = getDecisionByChoiceId cid $ decisions as
  return (c, os, d)
