
module Database (
  MediaDb, findDbFolder, initDb, openDb
  ) where

import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQL
import System.Directory ( createDirectory, doesDirectoryExist, getCurrentDirectory )
import System.FilePath ( (</>), takeDirectory )

import Paths_mdb

dbDir :: FilePath -> FilePath
dbDir base = base </> ".mdb"

data MediaDb = MDB
               { mdbConn :: SQL.Connection
               }

initDb :: FilePath -> IO ()
initDb p = do
  putStrLn $ "initializing mdb in " ++ (dbDir p)
  
  ex <- doesDirectoryExist (dbDir p)
  if ex
    then error $ "directory does already exist"
    else do
      createDirectory $ dbDir p
      initQuery <- getDataFileName "data/create-tables.sql" >>= TIO.readFile
      SQL.withConnection (dbDir p </> "index.db") $ \c -> do
        SQL.execute_ c $ SQL.Query initQuery
        return ()
    
  
openDb :: FilePath -> IO MediaDb
openDb dir = do
  c <- SQL.open (dir </> "index.db")
  return $ MDB c
    
-- | finds the DB folder relative to the current directory by walking
--   upwards the tree until a ".mdb" directory is found
findDbFolder :: IO (Maybe FilePath)
findDbFolder = getCurrentDirectory >>= go where
  go d = do
    putStrLn d
    here <- doesDirectoryExist $ dbDir d
    if here
      then return $ (Just $ dbDir d)
      else let d' = takeDirectory d in if (d' == d)
                                       then return Nothing
                                       else go d'
  
