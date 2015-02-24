
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    OverloadedStrings
    #-}

module Database (
    MediaDb, withMediaDb, findDbFolder, initDb, mdbBasePath, mdbDbDir,
    MDB, runMDB, runMDB', findDbAndRun,

    -- * files
    addFile, hasFile, listFiles,

    -- * streams
    clearStreams, addStream,

    -- * persons
    addPerson, listPersons
  ) where

import Control.Applicative ( Applicative, (<$>) )
import Control.Monad ( forM_ )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, bracket )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader ( MonadReader, ReaderT, asks, runReaderT )
import Data.Monoid ( (<>) )
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQL
import System.Directory ( createDirectory, doesDirectoryExist, getCurrentDirectory )
import System.FilePath ( (</>), makeRelative, takeDirectory )

import Paths_mdb
import Mdb.Database.File ( File, FileId )
import Mdb.Database.Person ( Person, PersonId )

dbDir :: FilePath -> FilePath
dbDir base = base </> ".mdb"

data MediaDb = MediaDb
    { mdbConn       :: ! SQL.Connection
    , mdbBasePath   :: ! FilePath
    , mdbDbDir      :: ! FilePath
    }

withMediaDb :: (Functor m, MonadIO m, MonadMask m) => (MediaDb -> m a) -> m a
withMediaDb a = liftIO findDbFolder >>= \x -> case x of
  Nothing  -> liftIO $ fail "no db directory found, maybe try \"mdb init\"?"
  Just dbf -> bracket (openDb dbf) closeDb a

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

runMDB' :: MediaDb -> MDB m a -> m a
runMDB' db f = runReaderT (unMDB f) db

findDbAndRun :: (MonadMask m, MonadIO m) => MDB m a -> m a
findDbAndRun act = liftIO findDbFolder >>= \x -> case x of
  Nothing  -> liftIO $ fail "no db directory found, maybe try \"mdb init\"?"
  Just dbf -> runMDB dbf act

dbTables :: [String]
dbTables =
    [ "file"
    , "stream"
    , "stream_metadata"
    , "person"
    , "person_file"
    ]

initDb :: FilePath -> IO ()
initDb p = do
    putStrLn $ "initializing mdb in " ++ (dbDir p)

    doesDirectoryExist (dbDir p) >>= \ex -> if ex
        then fail "directory does already exist"
        else do
            createDirectory $ dbDir p
            SQL.withConnection (dbDir p </> "index.db") $ \c ->
                forM_ dbTables $ \table -> do
                initFn <- getDataFileName $ "files/sql/create-table-" ++ table ++ ".sql"
                putStrLn $ "creating table " ++ table
                q <- TIO.readFile initFn
                SQL.execute_ c $ SQL.Query q

openDb :: MonadIO m => FilePath -> m MediaDb
openDb dir = do
  c <- liftIO $ SQL.open (dir </> "index.db")
  return $ MediaDb c (takeDirectory dir) dir

closeDb :: MonadIO m => MediaDb -> m ()
closeDb db = liftIO $ SQL.close $ mdbConn db

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
-- Files
-----------------------------------------------------------------

type StreamId = Integer

addFile :: MonadIO m => (FilePath, Integer) -> MDB m FileId
addFile (absPath, fs) = do
    relPath <- asks mdbBasePath >>= \bp -> return $ makeRelative bp absPath
    asks mdbConn >>= \c -> liftIO $ do
        SQL.execute c
            "INSERT INTO file (file_name, file_size) VALUES (?, ?)"
            (relPath, fs)

        SQL.query_ c "SELECT last_insert_rowid()" >>= return . SQL.fromOnly . head

hasFile :: MonadIO m => FilePath -> MDB m Bool
hasFile p = do
    relPath <- asks mdbBasePath >>= \bp -> return $ makeRelative bp p
    r <- asks mdbConn >>= \c -> liftIO $ SQL.query c
        "SELECT EXISTS(SELECT 1 FROM file WHERE file_name=? LIMIT 1)"
        (SQL.Only relPath)
    return $ (SQL.fromOnly . head) r

listFiles
    :: MonadIO m
    => Int -- ^ offset
    -> Int -- ^ count
    -> MDB m [File]
listFiles off cnt = do
    asks mdbConn >>= \c -> liftIO $ SQL.query c
        "SELECT file_id, file_name, file_size FROM file LIMIT ? OFFSET ?"
        (cnt, off)

clearStreams :: MonadIO m => FileId -> MDB m ()
clearStreams fid = asks mdbConn >>= \c -> liftIO $ SQL.execute c
    "DELETE FROM stream WHERE (file_id = ?)"
    (SQL.Only fid)

addStream :: MonadIO m => FileId -> StreamId -> (String, String, Int) -> MDB m ()
addStream fid sid (mt, cd, br) = asks mdbConn >>= \c -> liftIO $ SQL.execute c
    ("INSERT INTO stream"
        <> " (stream_id, file_id, stream_media_type, stream_codec, stream_bit_rate)"
        <> " VALUES (?, ?, ?, ?, ?)")
    (sid, fid, mt, cd, br)

-----------------------------------------------------------------
-- Persons
-----------------------------------------------------------------

addPerson :: MonadIO m => String -> MDB m PersonId
addPerson name = asks mdbConn >>= \c -> liftIO $ do
    SQL.execute c
        ("INSERT INTO person (person_name) VALUES (?)")
        (SQL.Only name)
    SQL.query_ c "SELECT last_insert_rowid()" >>= return . SQL.fromOnly . head

listPersons :: MonadIO m => Int -> Int -> MDB m [Person]
listPersons off cnt = do
    asks mdbConn >>= \c -> liftIO $ SQL.query c
        "SELECT person_id, person_name FROM person LIMIT ? OFFSET ?"
        (cnt, off)
