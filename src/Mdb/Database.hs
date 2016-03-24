
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Mdb.Database (
    MediaDb, findDbFolder, initDb, mdbBasePath, mdbDbDir,
    MDB, runMDB, runMDB', findDbAndRun, withTransaction,

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
    dbExecute, dbQuery, dbQueryOne, dbQuery_, dbLastRowId, SQL.Only(..)
  ) where

import           Control.Monad          (forM_, liftM)
import           Control.Monad.Catch    (MonadCatch, MonadMask, MonadThrow,
                                         bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, asks,
                                         runReaderT)
import           Data.Int               (Int64)
import           Data.Monoid            ((<>))
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
    { mdbConn     :: ! SQL.Connection
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

runMDB :: (MonadIO m, MonadMask m) => FilePath -> MDB m a -> m a
runMDB dbf act = bracket (liftIO $ openDb dbf) (liftIO . closeDb) (runReaderT (unMDB act))

runMDB' :: MediaDb -> MDB m a -> m a
runMDB' db f = runReaderT (unMDB f) db

findDbAndRun :: (MonadMask m, MonadIO m) => Maybe FilePath -> MDB m a -> m a
findDbAndRun mp act = do
    let
        goCheck p = do
            x <- if isRelative p
                then liftIO getCurrentDirectory >>= \here -> return $ here </> p
                else return p

            liftIO (doesDirectoryExist $ dbDir x) >>= \ok -> if ok
                then runMDB (dbDir x) act
                else fail $ "no db directory found at \"" ++ p ++ "\", maybe try \"mdb init\"?"

        goFind = liftIO findDbFolder >>= \x -> case x of
            Nothing  -> liftIO $ fail "no db directory found, maybe try \"mdb init\"?"
            Just dbf -> runMDB dbf act

    case mp of
        Nothing -> goFind
        Just p  -> goCheck p

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
    ]

initDb :: FilePath -> IO ()
initDb p = do
    putStrLn $ "initializing mdb in " ++ dbDir p

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
  liftIO $ SQL.execute_ c "PRAGMA foreign_keys = ON"
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
      then return (Just $ dbDir d)
      else let d' = takeDirectory d in if d' == d
                                       then return Nothing
                                       else go d'


dbExecute :: (MonadIO m, SQL.ToRow r) => SQL.Query -> r -> MDB m ()
dbExecute q r = asks mdbConn >>= \c -> liftIO $ SQL.execute c q r

dbQuery :: (MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> MDB m [r]
dbQuery q r = asks mdbConn >>= \c -> liftIO $ SQL.query c q r

dbQueryOne :: (MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> MDB m (Either T.Text r)
dbQueryOne q p = dbQuery q p >>= \ xs -> return $ case xs of
        []  -> Left "query returned no result but one was expected"
        [a] -> Right a
        _   -> Left "query returned multiple results but only one was expected"

dbQuery_ :: (MonadIO m, SQL.FromRow r) => SQL.Query -> MDB m [r]
dbQuery_ q = asks mdbConn >>= \c -> liftIO $ SQL.query_ c q

dbLastRowId :: (MonadIO m) => MDB m Int64
dbLastRowId = asks mdbConn >>= liftIO . SQL.lastInsertRowId

withTransaction :: MonadIO m => MDB IO a -> MDB m a
withTransaction f = ask >>= \mdb -> liftIO (SQL.withTransaction (mdbConn mdb) (runMDB' mdb f))

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

hasFile :: MonadIO m => FilePath -> MDB m Bool
hasFile p = do
    relPath <- relFile p
    r <- asks mdbConn >>= \c -> liftIO $ SQL.query c
        "SELECT EXISTS(SELECT 1 FROM file WHERE file_name=? LIMIT 1)"
        (SQL.Only relPath)
    return $ (SQL.fromOnly . head) r

fileById :: MonadIO m => FileId -> MDB m File
fileById fid = liftM head $ dbQuery
        "SELECT file_id, file_name, file_size, file_mime FROM file WHERE file_id=?"
        (SQL.Only fid)

setContainerInfo :: MonadIO m => FileId -> String -> Double -> MDB m ()
setContainerInfo fid fmtName duration = dbExecute
    (   "INSERT OR REPLACE INTO container"
    <>  " (file_id, container_duration, container_format)"
    <>  " VALUES (?, ?, ?)")
    (fid, duration, fmtName)

fileIdFromName :: MonadIO m => FilePath -> MDB m (Maybe FileId)
fileIdFromName fn = do
    relPath <- relFile fn
    asks mdbConn >>= \c -> liftIO $ SQL.query c
        "SELECT file_id FROM file WHERE file_name=? LIMIT 1"
        (SQL.Only relPath) >>= \ids -> case ids of
                                            [SQL.Only fid]  -> return $ Just fid
                                            _               -> return Nothing

assignFilePerson :: MonadIO m => FileId -> PersonId -> MDB m ()
assignFilePerson fid pid = asks mdbConn >>= \c -> liftIO $ SQL.execute c
    "INSERT OR IGNORE INTO person_file (person_id, file_id) VALUES (?, ?)"
    (pid, fid)

assignFileAlbum :: MonadIO m => FileId -> AlbumId -> MDB m ()
assignFileAlbum fid aid = dbExecute
    "INSERT OR IGNORE INTO album_file (album_id, file_id) VALUES (?, ?)"
    (aid, fid)

-----------------------------------------------------------------
-- Persons
-----------------------------------------------------------------

addPerson :: MonadIO m => String -> MDB m PersonId
addPerson name = dbExecute
    "INSERT INTO person (person_name) VALUES (?)"
    (SQL.Only name) >> dbLastRowId

-- | Get all files assigned to a person.
getPersonFiles :: MonadIO m => PersonId -> MDB m [File]
getPersonFiles pid = asks mdbConn >>= \c -> liftIO $ SQL.query c
    (   "SELECT f.file_id, f.file_name, f.file_size, file_mime FROM file f "
    <>  "NATURAL JOIN person_file "
    <>  "WHERE person_file.person_id = ?" )
    (SQL.Only pid)

----------------------------------------------------------
-- albums
----------------------------------------------------------

addAlbum :: MonadIO m => String -> MDB m AlbumId
addAlbum name = dbExecute "INSERT INTO album (album_name) VALUES (?)" (SQL.Only name) >> dbLastRowId

ensureTag :: MonadIO m => String -> MDB m TagId
ensureTag tag = withTransaction $
    dbQueryOne "SELECT tag_id FROM tag WHERE tag_name = ?" (SQL.Only tag) >>= \mtid -> case mtid of
        Right (SQL.Only tid)    -> return tid
        Left _                  -> do
            dbExecute "INSERT INTO tag(tag_name) VALUES (?)" (SQL.Only tag)
            dbLastRowId
