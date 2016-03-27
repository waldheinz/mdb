
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mdb.Serve.Video (
    videoApp, ensureFrame
    ) where

import           Blaze.ByteString.Builder   (Builder, fromByteString)
import           Control.Monad              (unless, void)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (asks)
import           Control.Monad.Trans.Class  (lift)
import           Data.Conduit
import           Data.Conduit.Process
import           Data.Fixed (divMod')
import           Data.Monoid                ((<>))
import qualified Data.Text as T
import           Data.Text.Encoding         (encodeUtf8)
import           Network.HTTP.Types         (status200)
import           Network.Wai
import           Network.Wai.Predicate
import           Network.Wai.Routing
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)

import           Mdb.Database
import           Mdb.Serve.Auth             (Authenticated)
import qualified Mdb.Serve.Auth             as AUTH
import           Mdb.Serve.Utils            (withFileAccess)
import           Mdb.Types

videoApp :: MediaDb -> AUTH.SessionKey IO -> Application
videoApp mdb skey req respond = runMDB' mdb $ route root req (liftIO . respond) where
    goAuth = AUTH.request skey req
    root = prepare $ do
        get "/:id/frame"        (continue $ goAuth . frame)        $ capture "id" .&. def 0 (query "ts")
        get "/:id/stream"       (continue $ goAuth . stream)       $
            capture "id" .&. def 0 (query "t") .&. opt (query "l") .&.  def 720 (query "rv")
        get "/:id/hls"          (continue $ goAuth . hls) $ capture "id"
        get "/:id/streamDirect" (continue $ goAuth . streamDirect) $ capture "id"

roundTimeToMs :: Double -> Integer
roundTimeToMs ts = round ts `div` 30 * 30000

ensureFrame :: (MonadMask m, MonadIO m) => FilePath -> FileId -> Double -> MDB m FilePath
ensureFrame p fid ts = do
    dbDir <- asks mdbDbDir

    let
        thumbDir    = dbDir ++ "/videoFrames/"
        tsMs = roundTimeToMs ts
        tsS  = fromIntegral tsMs / 1000 :: Double
        outFile = thumbDir ++ "/frame-" ++ show fid ++ "ts" ++ show tsMs ++ ".jpg"
        cmd = "ffmpeg -y -ss " ++ show tsS ++ " -i \"" ++ p ++ "\" -t 1 -f image2 -update 1 \"" ++ outFile ++ "\""
        createFrame = callCommand cmd

    exists <- liftIO $ doesFileExist outFile
    unless exists $ liftIO $ createDirectoryIfMissing True thumbDir >> createFrame
    return outFile

hls :: (MonadMask m, MonadIO m) => FileId -> Authenticated m Response
hls fid = withFileAccess go fid where
    buildm3u :: Double -> Builder
    buildm3u dur = start <> parts <> lastPart <> end where
        (partCount, lastLen) = (divMod' dur 10) :: (Int, Double)
        start = fromByteString $ encodeUtf8
            $   "#EXTM3U\n"
            <>  "#EXT-X-PLAYLIST-TYPE:VOD\n"
            <>  "#EXT-X-TARGETDURATION:10\n"
            <>  "#EXT-X-VERSION:3\n"
            <>  "#EXT-X-MEDIA-SEQUENCE:0\n"
        end = fromByteString $ encodeUtf8 "#EXT-X-ENDLIST\n"
        parts = mconcat $ map part [0..(partCount-1)] where
            part pt = fromByteString $ encodeUtf8 $ "#EXTINF:10.0,\nstream?t="
                <> T.pack (show $ pt * 10) <> "&l=10\n"
        lastPart = fromByteString $ encodeUtf8 $
            "#EXTINF:" <> T.pack (show lastLen) <> ",\nstream?t="
                <> T.pack (show $ partCount * 10) <> "&l=" <> T.pack (show lastLen) <>"\n"

    go _ _ = do
        mdur <- AUTH.queryOne "SELECT container_duration FROM container WHERE file_id = ?" (Only fid)
        case mdur of
            Left _ -> fail "container not found"
            Right (Only duration) -> do
                return $ responseBuilder status200
                            [("Content-Type", "application/x-mpegURL")] (buildm3u duration)

frame :: (MonadMask m, MonadIO m) => (FileId ::: Double) -> Authenticated m Response
frame (fid ::: ts) = withFileAccess go fid where
    go p _ = do
        outFile <- AUTH.unsafe $ ensureFrame p fid ts
        return $ responseFile status200 [("Content-Type", "image/jpeg")] outFile Nothing

stream :: (MonadMask m, MonadIO m) => (FileId ::: Double ::: Maybe Double ::: Int) -> Authenticated m Response
stream (fid ::: start ::: duration ::: rv) = withFileAccess go fid where
    go p _ = do
        let
            cmd = "ffmpeg -ss " ++ show start ++
                " -i \"" ++ p ++ "\"" ++
                (maybe "" (\l -> " -t " ++ show l) duration) ++
                " -vf scale=-2:" ++ show rv ++
                " -c:v libx264 -preset veryfast" ++
                " -f mpegts" ++
    --            " /tmp/out.mkv 2>&1"
                " - 2>/dev/null"

            str write flush = do
                void $ sourceCmdWithConsumer cmd $ awaitForever $ \bs -> lift $ write (fromByteString bs) >> flush
                flush

        return $ responseStream status200 [ ("Content-Type", "video/x-matroska") ] str

streamDirect :: (MonadMask m, MonadIO m) => FileId -> Authenticated m Response
streamDirect = withFileAccess $ \f mime ->
    return $ responseFile status200 [("Content-Type", encodeUtf8 mime)] f Nothing
