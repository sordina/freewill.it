
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DB.PostgresDB where

-- https://hackage.haskell.org/package/postgresql-simple-0.5.2.1/docs/Database-PostgreSQL-Simple.html

import API
import DB.Class
import Database.PostgreSQL.Simple

-- TODO: Possibly tunnel through PGDATABASE env variable somehow...
--       https://www.postgresql.org/docs/9.1/static/libpq-envars.html
connect :: IO Connection
connect = connectPostgreSQL "dbname='freewill'"

userids :: Connection -> IO [Int]
userids c = map fromOnly
        <$> query_ c "select userid from users"

-- DB.Class Instances

newtype PostgresConnection =
  PGC { getConnection :: Connection }

instance View PostgresConnection IO where
  view (PGC c) i = return $ CAD undefined [] Nothing

