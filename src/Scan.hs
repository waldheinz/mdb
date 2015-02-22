
module Scan (
  doScan
  ) where

import qualified Codec.FFmpeg.Decode as FFM
import qualified Codec.FFmpeg.Probe as FFM
import qualified Codec.FFmpeg.Types as FFM
import Control.Exception.Base ( IOException )
import Control.Monad ( forM, forM_ )
import Control.Monad.Catch ( MonadMask, MonadCatch, catchIOError )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Either
import Control.Monad.Error.Class ( catchError )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA ( bytestringDigest, sha1 )
import Data.Maybe ( catMaybes )
import System.Directory ( doesDirectoryExist, getCurrentDirectory, getDirectoryContents )
import System.FilePath ( (</>) )

import qualified CmdLine  as CMD
import Database

doScan :: (MonadMask m, MonadIO m) => CMD.OptScan -> MDB m ()
doScan CMD.OptScan = do
    here <- liftIO getCurrentDirectory
    traverseFiles here checkFile

checkFile :: (MonadMask m, MonadIO m) => FilePath -> MDB m ()
checkFile fn = do
    liftIO $ putStrLn fn
    (flip catchIOError)
        (\e -> (\x -> liftIO $ putStrLn $ "caught: " ++ show x) (e :: IOException))
        $ addStreamInfo fn
{-
        withFile fn ReadMode $ \h -> do
                -- contents <- BSL.hGetContents h
                size <- hFileSize h
                DB.addFile db (fn, size, Nothing)

                -}

traverseFiles :: MonadIO m => FilePath -> (FilePath -> m ()) -> m ()
traverseFiles fp act = do
  cs <- liftIO $ getDirectoryContents fp
  subs <- forM cs $ \c -> do
    (liftIO . doesDirectoryExist) (fp </> c) >>= \dir ->
      if dir
      then return $ Just c
      else act (fp </> c) >> return Nothing

  mapM_ (\sub -> traverseFiles (fp </> sub) act) $
    filter (\d -> (d /= "." && d /= ".." && d /= ".mdb")) $ catMaybes subs

addStreamInfo :: (MonadMask m, MonadIO m) => FilePath -> MDB m ()
addStreamInfo fn = FFM.withAvFile fn $ do
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
                liftIO $ putStrLn $ show (tn, cn, br)
                FFM.streamMetadata >>= FFM.dictFoldM_ (\x -> (liftIO . putStrLn . show) x)
                