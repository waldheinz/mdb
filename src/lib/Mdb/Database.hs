
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Mdb.Database (
    MediaDb, findDbFolder, initDb, mdbBasePath, mdbDbDir,
    MDB, runMDB, runMDB', findDbAndRun, withTransaction,
    isolate, withConnection,

    -- * files
    fileById, hasFile, fileIdFromName, assignFilePerson,
    fileAbs, relFile, assignFileAlbum,

    -- * videos / streams
    setContainerInfo,

    -- * persons
    addPerson, getPersonFiles,

    -- * albums
    addAlbum,

    -- * tags
    ensureTag,

    -- * raw queries
    dbExecute, dbExecute_, dbQuery, dbQueryOne, dbQuery_, dbLastRowId, SQL.Only(..)
  ) where

import           Control.Monad          (forM_, liftM)
import           Control.Monad.Catch    (MonadCatch, MonadMask, MonadThrow,
                                         bracket)
import qualified Control.Monad.Logger   as LOG
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, asks, local,
                                         runReaderT)
import           Data.Int               (Int64)
import           Data.Monoid            ((<>))
import           Data.Pool              (Pool, createPool, destroyAllResources, takeResource, putResource)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Database.SQLite.Simple as SQL
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         getCurrentDirectory)
import           System.FilePath        (isRelative, makeRelative,
                                         takeDirectory, (</>))

import           Mdb.Database.File      (File)
import           Mdb.Types
import           Paths_mdb

dbDir :: FilePath -> FilePath
dbDir base = base </> ".mdb"

data MediaDb = MediaDb
    { mdbConnPool :: Either (Pool SQL.Connection) SQL.Connection
    , mdbLogger   :: LOG.Loc -> LOG.LogSource -> LOG.LogLevel -> LOG.LogStr -> IO ()
    , mdbBasePath :: ! FilePath
    , mdbDbDir    :: ! FilePath
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

instance MonadIO m => LOG.MonadLogger (MDB m) where
    monadLoggerLog loc logsrc lvl msg = asks mdbLogger >>= \logger -> liftIO $ logger loc logsrc lvl (LOG.toLogStr msg)

instance MonadIO m => LOG.MonadLoggerIO (MDB m) where
    askLoggerIO = asks mdbLogger

runMDB :: (LOG.MonadLoggerIO m, MonadMask m) => FilePath -> MDB m a -> m a
runMDB dbf act = bracket (openDb dbf) closeDb (runReaderT $! unMDB act)

runMDB' :: MediaDb -> MDB m a -> m a
runMDB' db f = f `seq` runReaderT (unMDB f) db

findDbAndRun :: (MonadMask m, LOG.MonadLoggerIO m) => Maybe FilePath -> MDB m a -> m a
findDbAndRun mp act = maybe goFind goCheck mp where
    goCheck p = do
        x <- if isRelative p
            then liftIO getCurrentDirectory >>= \here -> return $ here </> p
            else return p

        liftIO (doesDirectoryExist $ dbDir x) >>= \ok -> if ok
            then runMDB (dbDir x) act
            else fail $ "no db directory found at \"" ++ p ++ "\", maybe try \"mdb init\"?"

    goFind = liftIO findDbFolder >>= \x -> case x of
        Nothing  -> fail "no db directory found, maybe try \"mdb init\"?"
        Just dbf -> runMDB dbf act

dbTables :: [String]
dbTables =
    [ "album"
    , "album_file"
    , "file"
    , "container"
    , "person"
    , "person_file"
    , "movie"
    , "movie_person"
    , "series"
    , "series_season"
    , "series_episode"
    , "stream"
    , "stream_metadata"
    , "tag"
    , "tag_file"
    , "user"
    , "user_session"
    , "user_tag_whitelist"
    , "user_video_play"
    ]

initDb :: (LOG.MonadLoggerIO m) =>  FilePath -> m ()
initDb p = do
    LOG.logInfoN $ "initializing mdb in " <> T.pack (dbDir p)

    liftIO $ doesDirectoryExist (dbDir p) >>= \ex -> if ex
        then fail "directory does already exist"
        else do
            createDirectory $ dbDir p
            SQL.withConnection (dbDir p </> "index.db") $ \c ->
                forM_ dbTables $ \table -> do
                    initFn <- getDataFileName $ "files/sql/create-table-" ++ table ++ ".sql"
                    q <- TIO.readFile initFn
                    SQL.execute_ c $ SQL.Query q

openDb :: (LOG.MonadLoggerIO m, MonadIO m) => FilePath -> m MediaDb
openDb dir = do
    logger <- LOG.askLoggerIO

    let
        create = do
            c <- SQL.open ("file:" ++ dir </> "index.db")
            SQL.execute_ c "PRAGMA foreign_keys = ON"
            SQL.execute_ c "PRAGMA journal_mode = WAL"
            LOG.runLoggingT (LOG.logDebugN "opened a DB connection") logger
            return c

        close c = SQL.close c >> LOG.runLoggingT (LOG.logDebugN "closed a DB connection") logger
        destroyWait = fromRational 30
        stripes = 1
        perStripe = 16

    pool <- liftIO $ createPool create close stripes destroyWait perStripe
    logFun <- LOG.askLoggerIO
    return $ MediaDb (Left pool) logFun (takeDirectory dir) dir

closeDb :: (LOG.MonadLogger m, MonadIO m) => MediaDb -> m ()
closeDb db = case mdbConnPool db of
    Left pool   -> liftIO (destroyAllResources pool)
    Right _     -> return ()

-- | finds the DB folder relative to the current directory by walking
--   upwards the tree until a ".mdb" directory is found
findDbFolder :: IO (Maybe FilePath)
findDbFolder = getCurrentDirectory >>= go where
  go d = do
    here <- doesDirectoryExist $ dbDir d
    if here
      then return (Just $ dbDir d)
      else let d' = takeDirectory d in if d' == d
                                       then return Nothing
                                       else go d'

withConnection :: (MonadMask m, MonadIO m) => (SQL.Connection -> MDB m a) -> MDB m a
withConnection f = do
    mdb <- ask

    case mdbConnPool mdb of
        Right c   -> f c
        Left pool -> bracket
            (liftIO $ takeResource pool)
            (\(c, lp) -> liftIO $ putResource lp c)
            (\(c, _) -> f c )

isolate :: (MonadIO m, MonadMask m) => MDB m a -> MDB m a
isolate f = ask >>= \mdb -> case mdbConnPool mdb of
    Right _ -> f
    Left _  -> withConnection $ \c -> local (const $ mdb { mdbConnPool = Right c } ) f

dbExecute :: (MonadMask m, MonadIO m, SQL.ToRow r) => SQL.Query -> r -> MDB m ()
dbExecute q r = withConnection (\c -> liftIO $! SQL.execute c q r)

dbExecute_ :: (MonadMask m, MonadIO m) => SQL.Query -> MDB m ()
dbExecute_ q = withConnection (\c -> liftIO $! SQL.execute_ c q)

dbQuery :: (MonadMask m, MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> MDB m [r]
dbQuery q r = withConnection $ \c -> do
    LOG.logDebugN $ SQL.fromQuery q
    liftIO $ SQL.query c q r

dbQueryOne :: (MonadMask m, MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> MDB m (Either T.Text r)
dbQueryOne q p = dbQuery q p >>= \ xs -> return $! case xs of
        []  -> Left "query returned no result but one was expected"
        [a] -> Right a
        _   -> Left "query returned multiple results but only one was expected"

dbQuery_ :: (MonadMask m, MonadIO m, SQL.FromRow r) => SQL.Query -> MDB m [r]
dbQuery_ q = withConnection $ \c -> liftIO $! SQL.query_ c q

dbLastRowId :: (MonadMask m, MonadIO m) => MDB m Int64
dbLastRowId = withConnection $! liftIO . SQL.lastInsertRowId

withTransaction :: (MonadMask m, MonadIO m) => MDB IO a -> MDB m a
withTransaction f = withConnection $ \c -> ask >>= \mdb -> liftIO $! SQL.withTransaction c (runMDB' mdb f)

-----------------------------------------------------------------
-- Files
-----------------------------------------------------------------

relFile :: MonadIO m => FilePath -> MDB m FilePath
relFile absPath = do
    -- rp <- liftIO $ canonicalizePath absPath
    x <- if isRelative absPath
        then liftIO getCurrentDirectory >>= \here -> return $ here </> absPath
        else return absPath

    asks mdbBasePath >>= \bp -> return $ makeRelative bp x

fileAbs :: Monad m => FilePath -> MDB m FilePath
fileAbs relPath = asks mdbBasePath >>= \base -> return $ base </> relPath

hasFile :: (MonadMask m, MonadIO m) => FilePath -> MDB m Bool
hasFile p = do
    relPath <- relFile p
    r <- dbQuery
        "SELECT EXISTS(SELECT 1 FROM file WHERE file_name=? LIMIT 1)"
        (SQL.Only relPath)
    return $ (SQL.fromOnly . head) r

fileById :: (MonadMask m, MonadIO m) => FileId -> MDB m File
fileById fid = liftM head $ dbQuery
        "SELECT file_id, file_name, file_size, file_mime FROM file WHERE file_id=?"
        (SQL.Only fid)

setContainerInfo :: (MonadMask m, MonadIO m) => FileId -> String -> Double -> MDB m ()
setContainerInfo fid fmtName duration = dbExecute
    (   "INSERT OR REPLACE INTO container"
    <>  " (file_id, container_duration, container_format)"
    <>  " VALUES (?, ?, ?)")
    (fid, duration, fmtName)

fileIdFromName :: (MonadMask m, MonadIO m) => FilePath -> MDB m (Maybe FileId)
fileIdFromName fn = do
    relPath <- relFile fn
    dbQuery
        "SELECT file_id FROM file WHERE file_name=? LIMIT 1"
        (SQL.Only relPath) >>= \ids -> case ids of
                                            [SQL.Only fid]  -> return $ Just fid
                                            _               -> return Nothing

assignFilePerson :: (MonadMask m, MonadIO m) => FileId -> PersonId -> MDB m ()
assignFilePerson fid pid = dbExecute
    "INSERT OR IGNORE INTO person_file (person_id, file_id) VALUES (?, ?)"
    (pid, fid)

assignFileAlbum :: (MonadMask m, MonadIO m) => FileId -> AlbumId -> MDB m ()
assignFileAlbum fid aid = dbExecute
    "INSERT OR IGNORE INTO album_file (album_id, file_id) VALUES (?, ?)"
    (aid, fid)

-----------------------------------------------------------------
-- Persons
-----------------------------------------------------------------

addPerson :: (MonadMask m, MonadIO m) => String -> MDB m PersonId
addPerson name = dbExecute
    "INSERT INTO person (person_name) VALUES (?)"
    (SQL.Only name) >> dbLastRowId

-- | Get all files assigned to a person.
getPersonFiles :: (MonadMask m, MonadIO m, LOG.MonadLogger m) => PersonId -> MDB m [File]
getPersonFiles pid = dbQuery
    (   "SELECT f.file_id, f.file_name, f.file_size, file_mime FROM file f "
    <>  "NATURAL JOIN person_file "
    <>  "WHERE person_file.person_id = ?" )
    (SQL.Only pid)

----------------------------------------------------------
-- albums
----------------------------------------------------------

addAlbum :: (MonadMask m, MonadIO m) => String -> MDB m AlbumId
addAlbum name = dbExecute "INSERT INTO album (album_name) VALUES (?)" (SQL.Only name) >> dbLastRowId

ensureTag :: (MonadMask m, MonadIO m) => String -> MDB m TagId
ensureTag tag = withTransaction $
    dbQueryOne "SELECT tag_id FROM tag WHERE tag_name = ?" (SQL.Only tag) >>= \mtid -> case mtid of
        Right (SQL.Only tid)    -> return tid
        Left _                  -> do
            dbExecute "INSERT INTO tag(tag_name) VALUES (?)" (SQL.Only tag)
            dbLastRowId
