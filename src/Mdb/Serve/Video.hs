
{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Mdb.Serve.Video (
    videoApp
    ) where

import           Blaze.ByteString.Builder ( fromByteString )
import           Control.Monad ( unless, void )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader.Class ( asks )
import           Control.Monad.Trans.Class ( lift )
import           Data.Conduit
import           Data.Conduit.Process
import           Network.HTTP.Types ( status200 )
import           Network.Wai
import           Network.Wai.Predicate
import           Network.Wai.Routing
import           System.Directory ( doesFileExist, createDirectoryIfMissing )

import           Mdb.Database
import qualified Mdb.Database.File as DBF

videoApp :: MediaDb -> Application
videoApp mdb req respond = runMDB' mdb $ route root req (liftIO . respond) where
    root = prepare $ do
        get "/:id/frame"        (continue frame)        $ capture "id" .&. query "ts"
        get "/:id/stream"       (continue stream)       $ capture "id" .&. query "t"
        get "/:id/streamDirect" (continue streamDirect) $ capture "id"

roundTimeToMs :: Double -> Integer
roundTimeToMs ts = round ts `div` 30 * 30000

frame :: MonadIO m => (DBF.FileId ::: Double) -> MDB m Response
frame (fid ::: ts) = do
    dbDir <- asks mdbDbDir
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f

    let
        thumbDir    = dbDir ++ "/videoFrames/"
        tsMs = roundTimeToMs ts
        tsS  = fromIntegral tsMs / 1000 :: Double
        outFile = thumbDir ++ "/frame-" ++ show fid ++ "ts" ++ show tsMs ++ ".jpg"
        cmd = "ffmpeg -y -ss " ++ show tsS ++ " -i \"" ++ p ++ "\" -t 1 -f image2 -update 1 \"" ++ outFile ++ "\""
        createFrame = callCommand cmd

    exists <- liftIO $ doesFileExist outFile
    unless exists $ liftIO $ createDirectoryIfMissing True thumbDir >> createFrame
    return $ responseFile status200 [] outFile Nothing

stream :: MonadIO m => (DBF.FileId ::: Double) -> MDB m Response
stream (fid ::: ts) = do
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f

    let
        cmd = "ffmpeg -ss " ++ show ts ++
            " -i \"" ++ p ++ "\"" ++
--            " -vf scale=-2:240" ++
            " -c:v libx264 -preset veryfast" ++
            " -f matroska" ++
--            " /tmp/out.mkv 2>&1"
            " - 2>/dev/null"
        str write flush = do
            void $ sourceCmdWithConsumer cmd $ awaitForever $ \bs -> lift $ write (fromByteString bs) >> flush
            flush

    return $ responseStream status200 [] str

streamDirect :: MonadIO m => DBF.FileId -> MDB m Response
streamDirect fid = do
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f
    return $ responseFile status200 [] p Nothing
