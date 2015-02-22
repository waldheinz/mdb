
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    OverloadedStrings
    #-}

module Database (
    findDbFolder, initDb,

    MDB, runMDB,

    -- * working with the DB
    addFile
  ) where

import Control.Applicative ( Applicative )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, bracket )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader ( MonadReader, ReaderT, asks, runReaderT )
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQL
import System.Directory ( createDirectory, doesDirectoryExist, getCurrentDirectory )
import System.FilePath ( (</>), makeRelative, takeDirectory )

import Paths_mdb

dbDir :: FilePath -> FilePath
dbDir base = base </> ".mdb"

data MediaDb = MediaDb
               { mdbConn     :: ! SQL.Connection
               , mdbBasePath :: ! FilePath
               }

newtype MDB m a = MDB { unMDB :: ReaderT MediaDb m a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadCatch
        , MonadIO
        , MonadMask
        , MonadReader MediaDb
        , MonadThrow
        )

runMDB :: (MonadIO m, MonadMask m) => FilePath -> MDB m a -> m a
runMDB dbf act = bracket (liftIO $ openDb dbf) (liftIO . closeDb) (runReaderT (unMDB act))

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
  return $ MediaDb c $ takeDirectory dir

closeDb :: MediaDb -> IO ()
closeDb db = SQL.close $ mdbConn db

-- | finds the DB folder relative to the current directory by walking
--   upwards the tree until a ".mdb" directory is found
findDbFolder :: IO (Maybe FilePath)
findDbFolder = getCurrentDirectory >>= go where
  go d = do
    here <- doesDirectoryExist $ dbDir d
    if here
      then return $ (Just $ dbDir d)
      else let d' = takeDirectory d in if (d' == d)
                                       then return Nothing
                                       else go d'

-----------------------------------------------------------------
-- working with the DB
-----------------------------------------------------------------

type FileInfo = (FilePath, Integer, Maybe BS.ByteString)

addFile :: MonadIO m => FileInfo -> MDB m ()
addFile (absPath, size, hash) = do
    relPath <- asks mdbBasePath >>= \bp -> return $ makeRelative bp absPath
    asks mdbConn >>= \c -> liftIO $ SQL.execute c
        "REPLACE INTO files (file_name, file_size, sha1) VALUES (?, ?, ?)"
        (relPath, size, hash)
