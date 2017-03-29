
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.PostgresDB where

-- https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html

import API
import DB.Class
import Data.List
import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ

-- TODO: Possibly tunnel through PGDATABASE env variable somehow...
--       https://www.postgresql.org/docs/9.1/static/libpq-envars.html
connectFreewill :: IO Connection
connectFreewill = connectPostgreSQL "dbname='freewill'"

userids :: Connection -> IO [Int]
userids c = map fromOnly
        <$> query_ c "select userid from users"

-- DB.Class Instances

newtype PostgresConnection =
  PGC { getConnection :: Connection }

data DecisionRow = DR {
  cid' :: ID,
  did' :: Maybe ID,
  oid' :: ID
  } deriving (Eq, Show, Generic)

instance FromRow DecisionRow

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
                        from decisions where decisionChoiceId = ?|]

-- query_ c "select userid from users" :: IO [Only Int]
--
-- data ChoiceAPIData
--   = CAD {theChoice :: Choice,
--          theOptions :: [Option],
--          theDecision :: Maybe Decision}
--
-- > connectFreewill >>= flip view 1 . PGC
instance View PostgresConnection IO where
  view c i = do
    cs <- postgresView c i
    print cs
    return $ CAD (Choice (Just 1) "hello") [] Nothing

