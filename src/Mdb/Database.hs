
{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    OverloadedStrings
    #-}

module Mdb.Database (
    MediaDb, findDbFolder, initDb, mdbBasePath, mdbDbDir,
    MDB, runMDB, runMDB', findDbAndRun, withTransaction,

    -- * files
    addFile, fileById, hasFile, fileIdFromName, assignFilePerson,
    fileAbs, relFile, assignFileAlbum,

    -- * videos / streams
    setContainerInfo,

    -- * persons
    addPerson, listPersons, getPerson, personImageFile, getPersonFiles,
    getAlbumPersons,

    -- * albums
    addAlbum,

    -- * raw queries
    dbExecute, dbQuery, dbQuery_, dbLastRowId, SQL.Only(..)
  ) where

import Control.Monad ( forM_, liftM )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, bracket )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Reader ( MonadReader, ReaderT, ask, asks, runReaderT )
import           Data.Int ( Int64 )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQL
import System.Directory ( createDirectory, doesDirectoryExist, getCurrentDirectory, canonicalizePath )
import System.FilePath ( (</>), makeRelative, takeDirectory )

import Paths_mdb
import Mdb.Database.Album ( AlbumId )
import Mdb.Database.File ( File, FileId )
import Mdb.Database.Person ( Person, PersonId )

dbDir :: FilePath -> FilePath
dbDir base = base </> ".mdb"

data MediaDb = MediaDb
    { mdbConn       :: ! SQL.Connection
    , mdbBasePath   :: ! FilePath
    , mdbDbDir      :: ! FilePath
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
        goCheck p = liftIO (doesDirectoryExist $ dbDir p) >>= \ok -> if ok
            then runMDB (dbDir p) act
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
    , "stream"
    , "stream_metadata"
    , "person"
    , "person_file"
    , "movie"
    , "movie_person"
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
    rp <- liftIO $ canonicalizePath absPath
    asks mdbBasePath >>= \bp -> return $ makeRelative bp rp

fileAbs :: Monad m => FilePath -> MDB m FilePath
fileAbs relPath = asks mdbBasePath >>= \base -> return $ base </> relPath

addFile :: MonadIO m => (FilePath, Integer, T.Text) -> MDB m FileId
addFile (absPath, fs, mime) = do
    relPath <- relFile absPath
    dbExecute
        "INSERT INTO file (file_name, file_size, file_mime) VALUES (?, ?, ?)"
        (relPath, fs, mime)
    dbLastRowId

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

getPerson :: MonadIO m => PersonId -> MDB m Person
getPerson pid = asks mdbConn >>= \c -> liftIO $ liftM head $ SQL.query c
        "SELECT person_id, person_name FROM person WHERE (person_id = ?)"
        (SQL.Only pid)

listPersons :: MonadIO m => Int -> Int -> MDB m [Person]
listPersons off cnt = asks mdbConn >>= \c -> liftIO $ SQL.query c
        "SELECT person_id, person_name FROM person LIMIT ? OFFSET ?"
        (cnt, off)

personImageFile :: Monad m => PersonId -> MDB m FilePath
personImageFile pid = asks $ \x -> mdbDbDir x ++ "/persons/images/" ++ show pid ++ ".jpg"

-- | Get all files assigned to a person.
getPersonFiles :: MonadIO m => PersonId -> MDB m [File]
getPersonFiles pid = asks mdbConn >>= \c -> liftIO $ SQL.query c
    (   "SELECT f.file_id, f.file_name, f.file_size, file_mime FROM file f "
    <>  "NATURAL JOIN person_file "
    <>  "WHERE person_file.person_id = ?" )
    (SQL.Only pid)

getAlbumPersons :: MonadIO m => AlbumId -> MDB m [Person]
getAlbumPersons aid = dbQuery
    ( "SELECT DISTINCT p.person_id, p.person_name FROM person p "
    <>  "NATURAL JOIN person_file "
    <>  "WHERE person_file.person_id = p.person_id AND EXISTS ("
    <>      "SELECT 1 FROM album a NATURAL JOIN person_file NATURAL JOIN album_file "
    <>          "WHERE person_file.person_id = p.person_id AND album_file.album_id = ?)"
    ) (SQL.Only aid)

----------------------------------------------------------
-- albums
----------------------------------------------------------

addAlbum :: MonadIO m => String -> MDB m AlbumId
addAlbum name = liftM fromIntegral $ dbExecute "INSERT INTO album (album_name) VALUES (?)" (SQL.Only name)
    >> dbLastRowId
