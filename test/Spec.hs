
import System.Exit

import qualified DB.MemDB as MemDB

failEither :: Show a => String -> Either a t -> IO ()
failEither m (Left  e) = print "Test Failed" >> print m >> print e >> exitFailure
failEither _ (Right _) = return ()

main :: IO ()
main = failEither "Testing MemDB Sanity Check" =<< MemDB.test

-- TODO: Comparative tests with DB
-- TODO: Simple error properties for double-decide, etc.
