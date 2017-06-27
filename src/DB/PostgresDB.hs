
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

newtype PostgresConnection m = PGC Connection

newPostgresDBConnection :: String -> IO (PostgresConnection m)
newPostgresDBConnection dbc = PGC <$> connectFreewill

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
instance MonadIO m => Share    (PC (m x)) m where share    (PGC db) uid cid       = liftIO $ postgresShare    db uid cid True
                                                  hide     (PGC db) uid cid       = liftIO $ postgresShare    db uid cid False
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
  [ myChoice ] <- query conn choiceQuery   (cid, uid)
  os           <- query conn optionQuery   (cid, uid, shared myChoice)
  ds           <- query conn decisionQuery (cid, uid, shared myChoice)
  let d         = makeDecision uid os ds
  return $ CAD myChoice os d
  where
  choiceQuery   = [sql| select choiceid, choicename, userid, shared
                        from choices where choiceid = ? and (userid = ? or shared = True)  |]
  optionQuery   = [sql| select optionChoiceId, optionid, optionName, userid
                        from options where optionChoiceId = ? and (userid = ? or ?)
                        order by created desc |]
  decisionQuery = [sql| select decisionChoiceId, decisionid, decision
                        from decisions where decisionChoiceId = ? and (userid = ? or ?) |]

postgresName :: Connection -> UserID -> Choice -> IO Choice
postgresName conn uid c = do
  [ Only cid ] <- query conn insertionQuery (choiceName c, uid)
  return $ c { choiceId = Just cid, choiceUserId = Just uid }
  where
  insertionQuery = [sql| insert into choices (choicename, userid) values (?,?) returning choiceid |]

verifyChoice :: Connection -> UserID -> ChoiceID -> IO Choice
verifyChoice conn uid cid = do
  [c] <- query conn selectionquery (uid, cid)
  return c
  where
  selectionquery = [sql| select choiceid, choicename, userid, shared
                         from choices where userid = ? and choiceid = ?  |]


postgresAdd :: Connection -> UserID -> ChoiceID -> Option -> IO Option
postgresAdd conn uid cid o = do
  _            <- verifyChoice conn uid cid
  let ocid      = optionChoiceId o
      _ouid     = optionUserId   o
  True         <- return $ ocid ==      cid
  [ ]          <- priorDecisions conn cid
  [ Only oid ] <- query conn insertionQuery (optionName o, ocid, uid)
  return $ o { optionId = Just oid, optionUserId = Just uid }
  where
  insertionQuery = [sql| insert into options (optionname, optionchoiceid, userid)
                         values (?,?,?) returning optionid |]

postgresShare :: Connection -> UserID -> ChoiceID -> Bool -> IO Choice
postgresShare conn uid cid s = do
  [c] <- query conn shareQuery (s, uid, cid)
  return c
  where
  shareQuery = [sql| update    choices set shared = ?
                     where     userid = ? and choiceid = ?
                     returning choiceid, choicename, userid, shared |]

postgresChoose :: Connection -> UserID -> ChoiceID -> OptionID -> IO Decision
postgresChoose conn uid cid oid = do
  []             <- priorDecisions conn cid
  [ Only did   ] <- query conn insertionQuery   (cid, oid, uid)
  [ Only oName ] <- query conn optionQuery      (oid, uid)
  let o           = Option cid (Just oid) oName (Just uid)
  return Decision { decisionId = did, decisionChoiceId = cid, decision = o, decisionUserId = Just uid }
  where
  insertionQuery = [sql| insert into decisions (decisionchoiceid, decision, userid) values (?,?,?) returning decisionid |]
  optionQuery    = [sql| select optionname from options where optionid = ? and userid = ? |]

priorDecisions :: Connection -> ChoiceID -> IO [ DecisionRow ]
priorDecisions conn cid = query conn [sql| select decisionchoiceid, decisionid, decision
                                           from decisions
                                           where decisionChoiceId = ? |] (Only cid)

postgresList :: Connection -> UserID -> IO [Choice]
postgresList conn uid = query conn selectionquery (Only uid)
  where
  selectionquery = [sql| select choiceid, choicename, userid, shared
                         from choices
                         where userid = ? or shared = True
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
  _            <- return                      (x :: [Only UserID])
  return (User uid rEmail)
  where
  lookupQuery    = [sql| select userid from users where email = ? |]
  insertionQuery = [sql| insert into users (email) values (?) returning userid |]
  updateQuery    = [sql| update users set password = md5(userid || '~' || ?) where userid = ? |]

postgresLogin :: Connection -> String -> Password -> IO User
postgresLogin conn rEmail (Password pass) = do
  [Only uid] <- query conn lookupQuery (rEmail, pass)
  return (User uid rEmail)
  where
  lookupQuery = [sql| select userid from users where email = ? and password = md5(userid || '~' || ?) |]
