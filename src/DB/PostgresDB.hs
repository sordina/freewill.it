
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.PostgresDB where

-- https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html

import API
import DB.Class
import Database.PostgreSQL.Simple

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

postgresView  :: PostgresConnection -> ID -> IO [(Integer, String)]
postgresView (PGC c) i =
  query c "select choiceid, choiceName from choices where choiceid = ?" (Only i)

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

