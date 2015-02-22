
module Scan (
  doScan
  ) where

import qualified Codec.FFmpeg.Decode as FFM
import qualified Codec.FFmpeg.Probe as FFM
import qualified Codec.FFmpeg.Types as FFM
import Control.Exception.Base ( IOException, catch )
import Control.Monad ( forM, forM_ )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.Either
import Control.Monad.Error.Class ( catchError )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA ( bytestringDigest, sha1 )
import Data.Maybe ( catMaybes )
import System.Directory ( doesDirectoryExist, getCurrentDirectory, getDirectoryContents )
import System.FilePath ( (</>) )
import System.IO

import qualified CmdLine  as CMD
import qualified Database as DB

doScan :: CMD.OptScan -> IO ()
doScan CMD.OptScan = DB.findDbFolder >>= \x -> case x of
  Nothing  -> putStrLn $ "no db directory found, maybe try \"mdb init\"?"
  Just dbf -> DB.withDbFolder dbf $ \db -> do
    here <- getCurrentDirectory
    traverseFiles here $ checkFile db

checkFile :: DB.MediaDb -> FilePath -> IO ()
checkFile db fn = do
    putStrLn fn
    (flip catch)
        (\e -> (\x -> putStrLn $ "caught: " ++ show x) (e :: IOException))
        $ addStreamInfo db fn
{-
        withFile fn ReadMode $ \h -> do
                -- contents <- BSL.hGetContents h
                size <- hFileSize h
                DB.addFile db (fn, size, Nothing)

                -}

traverseFiles :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseFiles fp act = do
  cs <- getDirectoryContents fp
  subs <- forM cs $ \c -> do
    doesDirectoryExist (fp </> c) >>= \dir ->
      if dir
      then return $ Just c
      else act (fp </> c) >> return Nothing

  mapM_ (\sub -> traverseFiles (fp </> sub) act) $
    filter (\d -> (d /= "." && d /= ".." && d /= ".mdb")) $ catMaybes subs

addStreamInfo :: DB.MediaDb -> FilePath -> IO ()
addStreamInfo _ fn = FFM.withAvFile fn $ do
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
                