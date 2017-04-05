
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.PostgresDB (connectFreewill) where

-- https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import API
import DB.Class
import Data.List
import GHC.Generics

-- TODO: Possibly tunnel through PGDATABASE env variable somehow...
--       Maybe this is automatic, maybe it isn't!
--       https://www.postgresql.org/docs/9.1/static/libpq-envars.html
connectFreewill :: IO Connection
connectFreewill = connectPostgreSQL "dbname='freewill'"

-- DB.Class Instances

instance View   PostgresConnection IO where view   = postgresView
instance Name   PostgresConnection IO where name   = postgresName
instance Add    PostgresConnection IO where add    = postgresAdd
instance Choose PostgresConnection IO where choose = postgresChoose
instance List   PostgresConnection IO where list   = postgresList

-- Helpers

newtype PostgresConnection = PGC Connection

data DecisionRow = DR {
  cid' :: ID,
  did' :: Maybe ID,
  oid' :: ID
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

postgresView  :: PostgresConnection -> ID -> IO ChoiceAPIData
postgresView (PGC conn) i = do
  [ theChoice ] <- query conn choiceQuery   (Only i) -- TODO: Introduce failure context
  os            <- query conn optionQuery   (Only i)
  ds            <- query conn decisionQuery (Only i)
  d             <- return $ makeDecision os ds
  return $ CAD theChoice os d
  where
  choiceQuery   = [sql| select choiceid, choicename
                        from choices where choiceid = ? |]
  optionQuery   = [sql| select optionChoiceId, optionid, optionName
                        from options where optionChoiceId = ? |]
  decisionQuery = [sql| select decision
                        from decisions where decisionChoiceId = ? |]

postgresName :: PostgresConnection -> Choice -> IO Choice
postgresName (PGC conn) c = do
  [ Only cid ] <- query conn insertionQuery (Only (choiceName c))
  return $ c { choiceId = Just cid }
  where
  insertionQuery = [sql| insert into choices (choicename) values (?) returning choiceid |]

-- TODO: Add checks for data security
postgresAdd :: PostgresConnection -> ID -> Option -> IO Option
postgresAdd (PGC conn) _cid o = do
  ocid         <- return (optionChoiceId o)
  [ Only oid ] <- query conn insertionQuery (optionName o, ocid)
  return $ o { optionId = Just oid }
  where
  insertionQuery = [sql| insert into options (optionname, optionchoiceid)
                         values (?,?) returning optionid |]

postgresChoose :: PostgresConnection -> ID -> ID -> IO Decision
postgresChoose (PGC conn) cid oid = do
  [ Only did   ] <- query conn insertionQuery (cid, oid)
  [ Only oName ] <- query conn optionQuery    (Only oid)
  o              <- return $ Option cid (Just oid) oName
  return $ Decision { decisionId = did, decisionChoiceId = cid, decision = o }
  where
  optionQuery    = [sql| select optionname from options where optionid = ? |]
  insertionQuery = [sql| insert into decisions (decisionchoiceid, decision)
                         values (?,?) returning decisionid |]

postgresList :: PostgresConnection -> IO [Choice]
postgresList (PGC conn) =
  query_ conn [sql| select choiceid, choicename from choices |]
