
module Mdb.File (
  doFile
  ) where

import qualified Codec.FFmpeg.Decode as FFM
import qualified Codec.FFmpeg.Probe as FFM
import qualified Codec.FFmpeg.Types as FFM
import Control.Exception.Base ( IOException )
import Control.Monad ( forM, forM_, unless, foldM )
import Control.Monad.Catch ( MonadMask, MonadCatch, catchIOError )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Either
import Control.Monad.Error.Class ( catchError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA ( bytestringDigest, sha1 )
import Data.Maybe ( catMaybes )
import Network.Mime ( defaultMimeLookup )
import System.Directory ( doesDirectoryExist, doesFileExist, getDirectoryContents )
import System.FilePath ( (</>) )
import System.IO ( IOMode(..), withFile, hFileSize )

import qualified Mdb.CmdLine  as CMD
import Database
import Mdb.Database.File ( FileId )

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

doFile (CMD.FileAdd) rec fs = mapM_ (withFiles go rec) fs where
    go fn = hasFile fn >>= \known -> unless known $ checkFile fn

ignoreFile :: FilePath -> Bool
ignoreFile d = d == "." || d == ".." || d == ".mdb"

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

checkFile :: (MonadMask m, MonadIO m) => FilePath -> MDB m ()
checkFile fn = do
    liftIO $ putStrLn fn
    (flip catchIOError)
        (\e -> (\x -> liftIO $ putStrLn $ "caught: " ++ show x) (e :: IOException))
        $ do
            sz <- liftIO $ withFile fn ReadMode $ \h -> do
                -- contents <- BSL.hGetContents h
                hFileSize h

            _ <- addFile (fn, sz, decodeUtf8 $ defaultMimeLookup $ T.pack fn)
            return ()

addStreamInfo :: (MonadMask m, MonadIO m) => FilePath -> FileId -> MDB m ()
addStreamInfo fn fid = FFM.withAvFile fn $ do
    FFM.formatName >>= liftIO . putStrLn
    FFM.formatMetadata >>= FFM.dictFoldM_ (\x -> (liftIO . putStrLn . show) x)
    ns <- FFM.nbStreams
    forM_ [0..(ns-1)] $ \sid -> FFM.withStream sid $ do
        mcctx <- FFM.codecContext
        case mcctx of
             Nothing -> liftIO $ putStrLn "no codec context"
             Just cctx -> do
                tn <- FFM.codecMediaTypeName cctx
                cn <- FFM.codecName cctx
                br <- FFM.streamBitrate cctx
                (lift . lift) $ addStream fid (fromIntegral sid) (tn, cn, br)
                liftIO $ putStrLn $ show (tn, cn, br)
                FFM.streamMetadata >>= FFM.dictFoldM_ (\x -> (liftIO . putStrLn . show) x)
                