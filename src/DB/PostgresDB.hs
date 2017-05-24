
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.PostgresDB
  ( connectFreewill
  , newPostgresDBConnection
  , PostgresConnection
  )
  where

-- https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Data.List
import GHC.Generics
import Control.Monad.IO.Class

import Data
import DB.Class

-- FUNDAMENTAL DATATYPE!!!!
--
newtype PostgresConnection m = PGC Connection

newPostgresDBConnection :: IO (PostgresConnection m)
newPostgresDBConnection = PGC <$> connectFreewill

-- TODO: Possibly tunnel through PGDATABASE env variable somehow...
--       Maybe this is automatic, maybe it isn't!
--       https://www.postgresql.org/docs/9.1/static/libpq-envars.html
connectFreewill :: IO Connection
connectFreewill = connectPostgreSQL "dbname='freewill'"

-- DB.Class Instances

type PC x = PostgresConnection x -- Shorthand

instance MonadIO m => View     (PC (m x)) m where view     (PGC db) uid cid       = liftIO $ postgresView     db uid cid
instance MonadIO m => Name     (PC (m x)) m where name     (PGC db) uid cdata     = liftIO $ postgresName     db uid cdata
instance MonadIO m => Add      (PC (m x)) m where add      (PGC db) uid cid odata = liftIO $ postgresAdd      db uid cid odata
instance MonadIO m => Choose   (PC (m x)) m where choose   (PGC db) uid cid oid   = liftIO $ postgresChoose   db uid cid oid
instance MonadIO m => List     (PC (m x)) m where list     (PGC db) uid           = liftIO $ postgresList     db uid
instance MonadIO m => Me       (PC (m x)) m where me       (PGC db) uid           = liftIO $ postgresMe       db uid
instance MonadIO m => Register (PC (m x)) m where register (PGC db)     fn ln     = liftIO $ postgresRegister db fn ln
instance MonadIO m => Login    (PC (m x)) m where login    (PGC db)     fn ln     = liftIO $ postgresLogin    db fn ln
instance MonadIO m => Database (PC (m x)) m

-- Helpers

data DecisionRow = DR {
  cid' :: ChoiceID,
  did' :: Maybe DecisionID,
  oid' :: OptionID
  } deriving (Eq, Show, Generic)

instance FromRow DecisionRow

-- Implementation

makeDecision :: UserID -> [Option] -> [DecisionRow] -> Maybe Decision
makeDecision _uid _os []   = Nothing
makeDecision _uid [] _ds   = Nothing
makeDecision uid  os (d:_) = case o of Just o' -> Just $ Decision cid did o' (Just uid)
                                       Nothing -> Nothing
  where
  oid = oid' d
  cid = cid' d
  did = did' d
  o   = find ((==Just oid).optionId) os

postgresView  :: Connection -> UserID -> ChoiceID -> IO ChoiceAPIData
postgresView conn uid cid = do
  [ myChoice ] <- query conn choiceQuery   (cid, uid) -- TODO: Introduce failure context
  os           <- query conn optionQuery   (cid, uid)
  ds           <- query conn decisionQuery (cid, uid)
  let d         = makeDecision uid os ds
  return $ CAD myChoice os d
  where
  choiceQuery   = [sql| select choiceid, choicename, userid
                        from choices where choiceid = ? and userid = ?  |]
  optionQuery   = [sql| select optionChoiceId, optionid, optionName, userid
                        from options where optionChoiceId = ? and userid = ?
                        order by created desc |]
  decisionQuery = [sql| select decisionChoiceId, decisionid, decision
                        from decisions where decisionChoiceId = ? and userid = ? |]

postgresName :: Connection -> UserID -> Choice -> IO Choice
postgresName conn uid c = do
  [ Only cid ] <- query conn insertionQuery (choiceName c, uid)
  return $ c { choiceId = Just cid, choiceUserId = Just uid }
  where
  insertionQuery = [sql| insert into choices (choicename, userid) values (?,?) returning choiceid |]

postgresAdd :: Connection -> UserID -> ChoiceID -> Option -> IO Option
postgresAdd conn uid _cid o = do
  let ocid      = optionChoiceId o
  [ Only oid ] <- query conn insertionQuery (optionName o, ocid, uid)
  return $ o { optionId = Just oid, optionUserId = Just uid }
  where
  insertionQuery = [sql| insert into options (optionname, optionchoiceid, userid)
                         values (?,?,?) returning optionid |]

postgresChoose :: Connection -> UserID -> ChoiceID -> OptionID -> IO Decision
postgresChoose conn uid cid oid = do
  [ Only did   ] <- query conn insertionQuery (cid, oid, uid)
  [ Only oName ] <- query conn optionQuery    (oid, uid)
  let o           = Option cid (Just oid) oName (Just uid)
  return Decision { decisionId = did, decisionChoiceId = cid, decision = o, decisionUserId = Just uid }
  where
  insertionQuery = [sql| insert into decisions (decisionchoiceid, decision, userid) values (?,?,?) returning decisionid |]
  optionQuery    = [sql| select optionname from options where optionid = ? and userid = ? |]

postgresList :: Connection -> UserID -> IO [Choice]
postgresList conn uid = query conn selectionquery (Only uid)
  where
  selectionquery = [sql| select choiceid, choicename, userid from choices
                         where userid = ?
                         order by created desc |]

postgresMe :: Connection -> UserID -> IO User
postgresMe conn uid = do
  [Only rEmail] <- query conn lookupQuery (Only uid)
  return (User uid rEmail)
  where
  lookupQuery = [sql| select email from users where userid = ? |]

postgresRegister :: Connection -> String -> Password -> IO User
postgresRegister conn rEmail (Password pass) = do
  x@[ ]        <- query   conn lookupQuery    (Only rEmail)
  [Only uid]   <- query   conn insertionQuery (Only rEmail)
  _            <- execute conn updateQuery    (pass, uid)
  types x >> return (User uid rEmail)
  where
  types :: [Only UserID] -> IO ()
  types _x       = return ()
  lookupQuery    = [sql| select userid from users where email = ? |]
  insertionQuery = [sql| insert into users (email) values (?) returning userid |]
  updateQuery    = [sql| update users set password = md5(userid || '~' || ?) where userid = ? |]

postgresLogin :: Connection -> String -> Password -> IO User
postgresLogin conn rEmail (Password pass) = do
  [Only uid] <- query conn lookupQuery (rEmail, pass)
  return (User uid rEmail)
  where
  lookupQuery = [sql| select userid from users where email = ? and password = md5(userid || '~' || ?) |]
