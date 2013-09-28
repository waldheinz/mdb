
module Database (
  MediaDb, findDbFolder, openDb
  ) where

import qualified Database.SQLite.Simple as SQL
import System.Directory ( doesDirectoryExist, getCurrentDirectory )
import System.FilePath ( (</>), takeDirectory )

data MediaDb = MDB
               { mdbConn :: SQL.Connection
               }

openDb :: FilePath -> IO MediaDb
openDb dir = do
  c <- SQL.open (dir </> "index.db")
  return $ MDB c
    
-- | finds the DB folder relative to the current directory by walking
--   upwards the tree until a ".mdb" directory is found
findDbFolder :: IO (Maybe FilePath)
findDbFolder = getCurrentDirectory >>= go where
  go d = do
    here <- doesDirectoryExist ".mdb"
    if here
      then return $ Just $ d </> ".mdb"
      else let d' = takeDirectory d in if (d' == d)
                                       then return Nothing
                                       else go $ takeDirectory d
  
