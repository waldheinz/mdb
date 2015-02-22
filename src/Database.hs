
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    OverloadedStrings
    #-}

module Database (
    findDbFolder, initDb,

    MDB, runMDB,

    -- * working with the DB
    File(..), FileId, addFile, hasFile, clearStreams, addStream
  ) where

import Control.Applicative ( Applicative )
import Control.Monad ( forM_ )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, bracket )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader ( MonadReader, ReaderT, asks, runReaderT )
import qualified Data.ByteString as BS
import Data.Monoid ( (<>) )
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

dbTables :: [String]
dbTables =
    [ "files"
    , "streams"
    , "stream_metadata"
    ]

initDb :: FilePath -> IO ()
initDb p = do
    putStrLn $ "initializing mdb in " ++ (dbDir p)

    doesDirectoryExist (dbDir p) >>= \ex -> if ex
        then error $ "directory does already exist"
        else do
            createDirectory $ dbDir p
            SQL.withConnection (dbDir p </> "index.db") $ \c ->
                forM_ dbTables $ \table -> do
                initFn <- getDataFileName $ "data/create-table-" ++ table ++ ".sql"
                putStrLn $ "creating table " ++ table
                q <- TIO.readFile initFn
                SQL.execute_ c $ SQL.Query q

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

data File = File
    { filePath :: ! FilePath
    , fileSize :: ! Integer
    , fileSha1 :: ! (Maybe BS.ByteString)
    }

type FileId = Integer
type StreamId = Integer

addFile :: MonadIO m => (FilePath, Integer) -> MDB m FileId
addFile (absPath, fs) = do
    relPath <- asks mdbBasePath >>= \bp -> return $ makeRelative bp absPath
    asks mdbConn >>= \c -> liftIO $ do
        SQL.execute c
            "INSERT INTO files (file_name, file_size) VALUES (?, ?)"
            (relPath, fs)

        SQL.query_ c "SELECT last_insert_rowid()" >>= return . SQL.fromOnly . head

hasFile :: MonadIO m => FilePath -> MDB m Bool
hasFile p = do
    relPath <- asks mdbBasePath >>= \bp -> return $ makeRelative bp p
    r <- asks mdbConn >>= \c -> liftIO $ SQL.query c
        "SELECT EXISTS(SELECT 1 FROM files WHERE file_name=? LIMIT 1)"
        (SQL.Only relPath)
    return $ (SQL.fromOnly . head) r

clearStreams :: MonadIO m => FileId -> MDB m ()
clearStreams fid = asks mdbConn >>= \c -> liftIO $ SQL.execute c
    "DELETE FROM streams WHERE (file_id = ?)"
    (SQL.Only fid)

addStream :: MonadIO m => FileId -> StreamId -> (String, String, Int) -> MDB m ()
addStream fid sid (mt, cd, br) = asks mdbConn >>= \c -> liftIO $ SQL.execute c
    ("INSERT INTO streams"
        <> " (stream_id, file_id, stream_media_type, stream_codec, stream_bit_rate)"
        <> " VALUES (?, ?, ?, ?, ?)")
    (sid, fid, mt, cd, br)
