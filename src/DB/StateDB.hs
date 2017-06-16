{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module DB.StateDB
  ( listState
  , addState
  , nameState
  , chooseState
  , viewState
  , shareState
  , registerState
  , meState
  , loginState
  , tryMaybe
  , LocalState(..)
  , emptyAppState
  , AppState
  )
  where

import DB.Class
import Data

import Data.List
import Servant
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import System.Random
import GHC.Generics
import Data.UUID
import Control.Lens (set, traversed, Indexable, (^.))
import Control.Lens.Fold
import Data.Generics.Record


-- Stateful store, re-exported type

data AppState = AS
  { options   :: [ Option ]
  , choices   :: [ Choice ]
  , decisions :: [ Decision ]
  , users     :: [ UserInfo ]
  , gen       :: StdGen
  } deriving (Show, Generic)


-- Internal user storage

data UserInfo = UserInfo
  { userId    :: UserID
  , userEmail :: String
  , userPass  :: Password
  } deriving (Show, Generic)


-- DB.Class Instance
-- Local State Version of Database that cannot persist outside of local scope.

data LocalState m = LS

instance MonadState AppState m                            => Name     (LocalState (m x)) m where name     LS = nameState
instance MonadState AppState m                            => List     (LocalState (m x)) m where list     LS = listState
instance (MonadError ServantErr m, MonadState AppState m) => Add      (LocalState (m x)) m where add      LS = addState
instance (MonadError ServantErr m, MonadState AppState m) => View     (LocalState (m x)) m where view     LS = viewState
instance (MonadError ServantErr m, MonadState AppState m) => Choose   (LocalState (m x)) m where choose   LS = chooseState
instance (MonadError ServantErr m, MonadState AppState m) => Register (LocalState (m x)) m where register LS = registerState
instance (MonadError ServantErr m, MonadState AppState m) => Login    (LocalState (m x)) m where login    LS = loginState
instance (MonadError ServantErr m, MonadState AppState m) => Me       (LocalState (m x)) m where me       LS = meState
instance (MonadError ServantErr m, MonadState AppState m) => Share    (LocalState (m x)) m where share    LS = shareState True
                                                                                                 hide     LS = shareState False

instance (MonadState AppState m, MonadError ServantErr m) => Database (LocalState (m x)) m where


-- Pure Implementations

getThing :: Eq a => (x -> Maybe a) -> a -> [x] -> Maybe x
getThing f oid = find ((== Just oid) . f)

getChoiceById :: ChoiceID -> [Choice] -> Maybe Choice
getChoiceById = getThing choiceId

getOptionById :: OptionID -> [Option] -> Maybe Option
getOptionById = getThing optionId

getOptionsByChoiceId :: UserID -> ChoiceID -> [Option] -> [Option]
getOptionsByChoiceId uid cid = filter test
  where
  test x = optionChoiceId x == cid
        && optionUserId   x == Just uid

getDecisionByChoiceId :: UserID -> ChoiceID -> [Decision] -> Maybe Decision
getDecisionByChoiceId uid cid = find test
  where
  test x = decisionChoiceId x == cid
        && decisionUserId   x == Just uid

tryMaybe :: MonadError ServantErr m => String -> Maybe a -> m a
tryMaybe _ (Just x) = return x
tryMaybe s Nothing  = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

tryBool :: MonadError ServantErr m => String -> Bool -> m ()
tryBool _ True  = return ()
tryBool s False = throwError (err404 {errReasonPhrase = "Not Found: " ++ s})

-- Stateful operations

nameState :: MonadState AppState m => UserID -> Choice -> m Choice
nameState uid cdata = do
  cs   <- choices <$> get
  cid  <- ChoiceID <$> newUUID
  let c = cdata { choiceId = Just cid, choiceUserId = Just uid }
  modify (\as -> as { choices = c : cs })
  return c

viewState :: (MonadError ServantErr m, MonadState AppState m) => UserID -> ChoiceID -> m ChoiceAPIData
viewState uid cid = do
  as    <- get
  c     <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid $ choices as
  let os = getOptionsByChoiceId uid cid $ options as
      d  = getDecisionByChoiceId uid cid $ decisions as
  return $ CAD c os d

select :: (Applicative f1, Traversable f, Indexable Int p) => (b -> Bool) -> p b (f1 b) -> f b -> f1 (f b)
select p = traversed . filtered p

shareState :: (MonadError ServantErr m, MonadState AppState m) => Bool -> UserID -> ChoiceID -> m Choice
shareState s uid cid = do
  as     <- get
  let p x = choiceId x == Just cid && choiceUserId x == Just uid
      nc  = set (select p . label @"shared") (Just s) (choices as)
      na  = as { choices = nc }
  c      <- tryMaybe ("Couldn't find choice " ++ show cid) $ firstOf (select p) nc
  put na
  return c

addState :: (MonadError ServantErr m, MonadState AppState m) => UserID -> ChoiceID -> Option -> m Option
addState uid cid' odata = do
  let cid = optionChoiceId odata
      msg = "choiceId " ++ show cid
         ++ " doesn't match choiceId " ++ show cid'
         ++ "... Try checking the JSON and route."

  -- Verify that choice referenced exists
  -- And that only one ID is referenced
  cs     <- choices <$> get
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid cs
  _      <- tryBool msg (cid == cid')
  oid    <- OptionID  <$> newUUID
  ds     <- decisions <$> get
  os     <- options   <$> get

  let exD = getDecisionByChoiceId uid cid ds
      o   = odata { optionId = Just oid, optionUserId = Just uid }

  -- Can't deliberate after choice
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  modify (\as -> as { options = o : os })
  return o

chooseState :: (MonadError ServantErr m, MonadState AppState m) => UserID -> ChoiceID -> OptionID -> m Decision
chooseState uid cid oid = do
  cs     <- choices <$> get
  _      <- tryMaybe ("Couldn't find choice " ++ show cid) $ getChoiceById cid cs
  os     <- options <$> get
  o      <- tryMaybe ("Couldn't find option " ++ show oid) $ getOptionById oid os
  did    <- DecisionID <$> newUUID
  ds     <- decisions  <$> get

  let exD = getDecisionByChoiceId uid cid ds
      d   = Decision cid (Just did) o (Just uid)

  -- Can't rechoose
  when (isJust exD) $ throwError (err403 {errReasonPhrase = "Already made a decision for this choice!"})

  modify (\as -> as { decisions = d : ds })
  return d

listState :: MonadState AppState m => UserID -> m [Choice]
listState uid = do
  let test x = choiceUserId x == Just uid || fromMaybe False (shared x)
  as        <- get
  return $ filter test $ choices as

meState :: (MonadState AppState m, MonadError ServantErr m) => UserID -> m User
meState uid = do
  us <- users <$> get
  let u = userInfoToUser <$> find (\x -> getField @"userId" x == uid) us
  tryMaybe "No user... Weird." u

registerState :: (MonadError ServantErr m, MonadState AppState m) => String -> Password -> m User
registerState rEmail pass = do
  us   <- users <$> get
  let u = find (\x -> getField @"userEmail" x == rEmail) us

  when (isJust u) $ throwError (err401 {errReasonPhrase = "User already registered..."})

  uid  <- UserID <$> newUUID
  let n = UserInfo uid rEmail pass

  modify (\as -> as { users = n : us })

  return $ userInfoToUser n

loginState :: (MonadError ServantErr m, MonadState AppState m) => String -> Password -> m User
loginState rEmail pass = do
  us <- users <$> get
  u  <- tryMaybe "Couldn't log-in" $ find (\x -> getField @"userEmail" x == rEmail && userPass x == pass) us
  return $ userInfoToUser u

userInfoToUser :: UserInfo -> User
userInfoToUser = (^. super @User)

newUUID :: MonadState AppState m => m UUID
newUUID = do
  g1 <- gen <$> get
  let (u, g2) = random g1
  modify (\as -> as { gen = g2 })
  return u

emptyAppState :: AppState
emptyAppState = AS [] [] [] [] (mkStdGen 293874928374)
