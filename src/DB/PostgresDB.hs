
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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

import API
import DB.Class

newPostgresDBConnection :: IO (PostgresConnection m)
newPostgresDBConnection = PGC <$> connectFreewill

-- TODO: Possibly tunnel through PGDATABASE env variable somehow...
--       Maybe this is automatic, maybe it isn't!
--       https://www.postgresql.org/docs/9.1/static/libpq-envars.html
connectFreewill :: IO Connection
connectFreewill = connectPostgreSQL "dbname='freewill'"

-- DB.Class Instances

instance MonadIO m => View   (PostgresConnection (m x)) m where view   (PGC db) cid       = liftIO $ postgresView    db cid
instance MonadIO m => Name   (PostgresConnection (m x)) m where name   (PGC db) cdata     = liftIO $ postgresName    db cdata
instance MonadIO m => Add    (PostgresConnection (m x)) m where add    (PGC db) cid odata = liftIO $ postgresAdd     db cid odata
instance MonadIO m => Choose (PostgresConnection (m x)) m where choose (PGC db) cid oid   = liftIO $ postgresChoose  db cid oid
instance MonadIO m => List   (PostgresConnection (m x)) m where list   (PGC db)           = liftIO $ postgresList    db

instance MonadIO m => Database (PostgresConnection (m x)) m

-- Helpers

newtype PostgresConnection m = PGC Connection

data DecisionRow = DR {
  cid' :: ChoiceID,
  did' :: Maybe DecisionID,
  oid' :: OptionID
  } deriving (Eq, Show, Generic)

instance FromRow DecisionRow

-- Implementation

makeDecision :: [Option] -> [DecisionRow] -> Maybe Decision
makeDecision _os []   = Nothing
makeDecision [] _ds   = Nothing
makeDecision os (d:_) = case o of Just o' -> Just $ Decision cid did o'
                                  Nothing -> Nothing
  where
  oid = oid' d
  cid = cid' d
  did = did' d
  o   = find ((==Just oid).optionId) os

postgresView  :: Connection -> ChoiceID -> IO ChoiceAPIData
postgresView conn i = do
  [ theChoice ] <- query conn choiceQuery   (Only i) -- TODO: Introduce failure context
  os            <- query conn optionQuery   (Only i)
  ds            <- query conn decisionQuery (Only i)
  d             <- return $ makeDecision os ds
  return $ CAD theChoice os d
  where
  choiceQuery   = [sql| select choiceid, choicename
                        from choices where choiceid = ?  |]
  optionQuery   = [sql| select optionChoiceId, optionid, optionName
                        from options where optionChoiceId = ?
                        order by optionid desc |]
  decisionQuery = [sql| select decisionChoiceId, decisionid, decision
                        from decisions where decisionChoiceId = ? |]

{-
viewTest :: IO ChoiceAPIData
viewTest = do
  db <- connectFreewill
  postgresView db (ChoiceID 1)

viewTestChoice :: IO Choice
viewTestChoice = do
  db <- connectFreewill
  [x :: Choice] <- query_ db [sql| select choiceid, choicename from choices where choiceid = 1 |]
  return x

viewTestOptions :: IO [Option]
viewTestOptions = do
  db <- connectFreewill
  (x :: [Option]) <- query_ db [sql| select optionChoiceId, optionId, optionName from options where optionchoiceid = 1 |]
  return x

viewTestDecision :: IO DecisionRow
viewTestDecision = do
  db <- connectFreewill
  [x :: DecisionRow] <- query_ db [sql| select decisionChoiceId, decisionid, decision from decisions where decisionChoiceId = 1 |]
  return x
-}

postgresName :: Connection -> Choice -> IO Choice
postgresName conn c = do
  [ Only cid ] <- query conn insertionQuery (Only (choiceName c))
  return $ c { choiceId = Just cid }
  where
  insertionQuery = [sql| insert into choices (choicename) values (?) returning choiceid |]

-- TODO: Add checks for data security
postgresAdd :: Connection -> ChoiceID -> Option -> IO Option
postgresAdd conn _cid o = do
  ocid         <- return (optionChoiceId o)
  [ Only oid ] <- query conn insertionQuery (optionName o, ocid)
  return $ o { optionId = Just oid }
  where
  insertionQuery = [sql| insert into options (optionname, optionchoiceid)
                         values (?,?) returning optionid |]

{-
 decisionid       | integer | not null default nextval('decisions_decisionid_seq'::regclass)
 userid           | integer |
 decisionchoiceid | integer |
 decision         | integer |
-}
postgresChoose :: Connection -> ChoiceID -> OptionID -> IO Decision
postgresChoose conn cid oid = do
  [ Only did   ] <- query conn insertionQuery (cid, oid)
  [ Only oName ] <- query conn optionQuery    (Only oid)
  o              <- return $ Option cid (Just oid) oName
  return $ Decision { decisionId = did, decisionChoiceId = cid, decision = o }
  where
  insertionQuery = [sql| insert into decisions (decisionchoiceid, decision) values (?,?) returning decisionid |]
  optionQuery    = [sql| select optionname from options where optionid = ? |]

chooseTest :: IO Decision
chooseTest = do
  db <- connectFreewill
  postgresChoose db (ChoiceID 1) (OptionID 1)

postgresList :: Connection -> IO [Choice]
postgresList conn =
  query_ conn [sql| select choiceid, choicename from choices
                    order by choiceid desc |]
