
{-# LANGUAGE OverloadedStrings #-}

module Mdb.File (
  doFile, checkFile
  ) where

import qualified Codec.FFmpeg.Probe as FFM
import Control.Exception.Base ( IOException )
import Control.Monad ( forM_, unless, foldM, when, liftM )
import Control.Monad.Catch ( MonadMask, MonadCatch, catchIOError )
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class ( lift )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16.Lazy as HEX
import Data.Digest.Pure.SHA ( bytestringDigest, sha1 )
import Network.Mime ( defaultMimeLookup )
import System.Directory ( doesDirectoryExist, doesFileExist, getDirectoryContents )
import System.FilePath ( (</>) )
import System.Posix.Files ( fileSize, getFileStatus )
import Text.Regex.Posix

import qualified Mdb.CmdLine  as CMD
import Mdb.Database
import Mdb.Database.File ( FileId, fileMime )
import Mdb.Types

doFile :: CMD.OptFile -> Bool -> [FilePath] -> MDB IO ()
doFile (CMD.FileAssign tgts) rec fs = withTransaction $ do
    let
        prepare (ps, as) tgt = case tgt of
            CMD.AssignPerson pid    -> return (pid : ps, as)
            CMD.AssignNewPerson n   -> addPerson n >>= \pid -> return (pid:ps, as)
            CMD.AssignAlbum aid     -> return (ps, aid:as)
            CMD.AssignNewAlbum n    -> addAlbum n >>= \aid -> return (ps, aid:as)

        go pids aids fn = do
            mfid <- fileIdFromName fn
            case mfid of
                Nothing    -> fail $ fn ++ " not registered yet"
                Just fid   -> do
                    mapM_ (assignFilePerson fid) pids
                    mapM_ (assignFileAlbum fid) aids

    (pids, aids) <- foldM prepare ([], []) tgts
    mapM_ (withFiles (go pids aids) rec) fs

doFile CMD.FileAdd rec fs = withTransaction $ mapM_ (withFiles go rec) fs where
    go fn = hasFile fn >>= \known -> unless known $ checkFile fn >>= \efid ->
        case efid of
            Left e      -> liftIO $ putStrLn $ fn ++ ": " ++ T.unpack e
            Right fid   -> liftIO $ putStrLn $ fn ++ ": " ++ show fid

doFile (CMD.FileScan sha) rec fs = mapM_ (withFiles (scanFile sha) rec) fs

scanFile :: MonadIO m => Bool -> FilePath -> MDB m ()
scanFile sha fn = withTransaction $ do
    mfid <- fileIdFromName fn
    case mfid of
        Nothing    -> liftIO $ putStrLn $ "unregistered file: " ++ fn
        Just fid   -> do
            when sha $ do
                hash <- liftIO $ liftM (bytestringDigest . sha1) (BSL.readFile fn)
                dbExecute "UPDATE file SET file_sha1=? WHERE file_id=?" (hash, fid)
                liftIO $ putStrLn $ fn ++ ":" ++ show (HEX.encode hash)

            f <- fileById fid

            when ("video" `T.isPrefixOf` fileMime f) $ do
                assignSeriesEpisode fn fid
                addVideoInfo fn fid

ignoreFile :: FilePath -> Bool
ignoreFile d = d == "." || d == ".." || d == ".mdb"

addFile :: MonadIO m => (FilePath, Integer, T.Text) -> MDB m FileId
addFile (absPath, fs, mime) = do
    relPath <- relFile absPath
    dbExecute
        (   "INSERT OR REPLACE INTO file (file_id, file_name, file_size, file_mime) "
        <>  "VALUES ((SELECT file_id FROM file WHERE file_name = ?), ?, ?, ?)"
        )
        (relPath, relPath, fs, mime)
    dbLastRowId

filteredContents :: MonadIO m => FilePath -> m [FilePath]
filteredContents fp = do
    c <- liftIO $ getDirectoryContents fp
    return $ filter (not . ignoreFile) c

withFiles :: MonadIO m => (FilePath -> m ()) -> Bool -> FilePath -> m ()
withFiles f rec fp = unless (ignoreFile fp) $ do
    isDir <- liftIO $ doesDirectoryExist fp
    if isDir
        then if rec
            then filteredContents fp >>= mapM_ (\fp' -> withFiles f rec $ fp </> fp')
            else liftIO $ putStrLn $ "ignoring directory " ++ fp
        else do
            isFile <- liftIO $ doesFileExist fp
            if isFile
                then f fp
                else liftIO $ putStrLn $ "neither file nor directory: " ++ fp

checkFile :: (MonadCatch m, MonadIO m) => FilePath -> MDB m (Either T.Text FileId)
checkFile fn = flip catchIOError
    (\e -> (\x -> return $ Left $ T.pack $ "caught: " ++ show x) (e :: IOException))
    $ do
        sz <- liftIO $ fromIntegral . fileSize <$> getFileStatus fn
        Right <$> addFile (fn, sz, decodeUtf8 $ defaultMimeLookup $ T.pack fn)

assignSeriesEpisode :: (MonadMask m, MonadIO m) => FilePath -> FileId -> MDB m ()
assignSeriesEpisode fn fid = do
    rel <- relFile fn

    let
        regex = "[S,s]([0-9]+)[E,e]([0-9]+)" :: String
        noParse = liftIO $ putStrLn $ "could not extract season / episode from \"" ++ fn ++ "\""
        assign serId seaId epId = do
            liftIO $ print (serId :: SerialId, seaId :: SeasonId, epId :: EpisodeId)
            dbExecute
                (   "UPDATE series_episode SET file_id = ? "
                <>  "WHERE series_id = ? AND series_season_number = ? AND series_episode_number = ?"
                ) (fid, serId, seaId, epId)
            return ()

    -- check if this file is within some series root and has a name we understand
    esid <- dbQueryOne "SELECT series_id FROM series WHERE ? LIKE (series_root || '%')" (Only rel)
    case esid of
        Left _              -> return ()
        Right (Only serId)  -> case rel =~ regex of
            [[_, seaIds, epIds]] -> case (reads seaIds, reads epIds) of
                ([(seaId, _)], [(epId, _)]) -> assign serId seaId epId
                _   -> noParse
            _  -> noParse

addVideoInfo :: (MonadMask m, MonadIO m) => FilePath -> FileId -> MDB m ()
addVideoInfo fn fid = FFM.withAvFile fn $ do
    (fmtName, duration) <- (,) <$> FFM.formatName <*> FFM.duration
    let
        durationSeconds = fromIntegral duration / 1000000

    lift $ setContainerInfo fid fmtName durationSeconds

    lift $ dbExecute "DELETE FROM stream WHERE (file_id = ?)" (Only fid)

    ns <- FFM.nbStreams
    forM_ [0..(ns-1)] $ \sid -> FFM.withStream sid $ do
        mcctx <- FFM.codecContext
        case mcctx of
             Nothing -> liftIO $ putStrLn "no codec context"
             Just cctx -> do
                tn <- FFM.codecMediaTypeName cctx
                cn <- FFM.codecName cctx
                br <- FFM.streamBitrate cctx
                (sw, sh) <- FFM.streamImageSize cctx
                (lift . lift) $ dbExecute
                    ("INSERT INTO stream"
                    <> " (stream_id, file_id, stream_media_type, stream_codec, stream_bit_rate, stream_width, stream_height)"
                    <> " VALUES (?, ?, ?, ?, ?, ?, ?)")
                        (sid, fid, tn, cn, br, sw, sh)

    liftIO $ putStrLn $ fn ++ ": " ++ show ns ++ " streams, " ++ show (round durationSeconds :: Int) ++ " seconds"
