
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mdb.Serve.Video (
    videoApp
    ) where

import           Blaze.ByteString.Builder   (Builder, fromByteString)
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
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

import           Mdb.Database
import           Mdb.Image                  (ensureFrame )
import           Mdb.Serve.Auth             (Authenticated)
import qualified Mdb.Serve.Auth             as AUTH
import           Mdb.Serve.Utils            (withFileAccess)
import           Mdb.Types

videoApp :: MediaDb -> AUTH.SessionKey IO -> Application
videoApp mdb skey req respond = runMDB' mdb $ route root req (liftIO . respond) where
    goAuth = AUTH.request skey req
    root = prepare $ do
        get "/:id/frame"        (continue $ goAuth . frame)        $ capture "id" .&. def 0 (query "ts")
        get "/:id/variants"     (continue $ goAuth . variants)     $ capture "id"
        get "/:id/stream"       (continue $ goAuth . stream)
            $   capture "id"
            .&. def 0 (query "t")
            .&. opt (query "end")
            .&. def 720 (query "rv")
            .&. def 4000 (query "bv")
            .&. def 96 (query "ba")
            .&. def "webm" (query "format")
        get "/:id/hls"          (continue $ goAuth . hls)
            $ capture "id" .&. def 480 (query "rv") .&. def 2000 (query "bv") .&. def 64 (query "ba")
        get "/:id/streamDirect" (continue $ goAuth . streamDirect) $ capture "id"

segmentDuration :: Int
segmentDuration = 10

variants :: (MonadMask m, MonadIO m) => FileId -> Authenticated m Response
variants = withFileAccess go where
    vs :: [(Int, Int, Int)]
    vs =
        [ (540  , 2000  , 96)
        , (270  , 400   , 64)
        , (360  , 600   , 64)
        , (360  , 1200  , 96)
        , (720  , 3000  , 96)
        , (720  , 4500  , 96)
        , (1080 , 5500  , 128)
        , (1080 , 7000  , 128)
        ]
    gov (rv, bv, ba) = fromByteString $ encodeUtf8
        $   "#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=" <> T.pack (show $ 1000 * (bv + ba)) <> "\n"
        <>  "hls?bv=" <> T.pack (show bv) <> "&ba=" <> T.pack (show ba) <> "&rv=" <> T.pack (show rv)
        <>  "\n"
    start = fromByteString $ encodeUtf8 "#EXTM3U\n"
    buildVariants = start <> mconcat (map gov vs)
    go _ _ = return $ responseBuilder status200 [("Content-Type", "application/x-mpegURL")] buildVariants

hls :: (MonadMask m, MonadIO m) => (FileId ::: Int ::: Int ::: Int) -> Authenticated m Response
hls (fid ::: rv ::: bv ::: ba) = withFileAccess go fid where
    buildm3u :: Double -> Builder
    buildm3u dur = start <> parts <> lastPart <> end where
        (partCount, lastLen) = divMod' dur (fromIntegral segmentDuration) :: (Int, Double)
        start = fromByteString $ encodeUtf8
            $   "#EXTM3U\n"
            <>  "#EXT-X-PLAYLIST-TYPE:VOD\n"
            <>  "#EXT-X-TARGETDURATION:" <> T.pack (show segmentDuration) <>"\n"
            <>  "#EXT-X-VERSION:3\n"
            <>  "#EXT-X-MEDIA-SEQUENCE:0\n"
        end = fromByteString $ encodeUtf8 "#EXT-X-ENDLIST\n"
        parts = mconcat $ map part [0..(partCount-1)] where  -- #EXT-X-DISCONTINUITY\n
            part pt = fromByteString $ encodeUtf8 $ "#EXTINF:" <> T.pack (show segmentDuration) <>",\nstream?t="
                <> T.pack (show $ pt * segmentDuration) <> "&end=" <> T.pack (show ((pt + 1) * segmentDuration))
                <> "&rv=" <> T.pack (show rv)
                <> "&bv=" <> T.pack (show bv)
                <> "&ba=" <> T.pack (show ba)
                <> "\n"
        lastPart = fromByteString $ encodeUtf8 $
            "#EXTINF:" <> T.pack (show lastLen) <> ",\nstream?t="
                <> T.pack (show $ partCount * segmentDuration) <> "&l=" <> T.pack (show lastLen) <>"\n"

    go _ _ = do
        mdur <- AUTH.queryOne "SELECT container_duration FROM container WHERE file_id = ?" (Only fid)
        case mdur of
            Left _ -> fail "container not found"
            Right (Only duration) ->
                return $ responseBuilder status200 [("Content-Type", "application/x-mpegURL")] (buildm3u duration)

frame :: (MonadMask m, MonadIO m) => (FileId ::: Double) -> Authenticated m Response
frame (fid ::: ts) = withFileAccess go fid where
    go p _ = do
        outFile <- AUTH.unsafe $ ensureFrame p fid ts
        return $ responseFile status200 [("Content-Type", "image/jpeg")] outFile Nothing

stream
    :: (MonadMask m, MonadIO m) => (FileId ::: Double ::: Maybe Double ::: Int ::: Int ::: Int ::: String)
    -> Authenticated m Response
stream (fid ::: start ::: end ::: rv ::: bv ::: ba ::: fmt) = withFileAccess go fid where
    go p _ = do
        let
            input =
                "ffmpeg -nostdin -loglevel quiet" ++
                " -ss " ++ show start ++
                " -i \"" ++ p ++ "\"" ++
                maybe "" (\l -> " -to " ++ show l) end ++
                " -vf scale=-2:" ++ show rv

            transcode = case fmt of
                "mkv"   ->
                    " -c:v libx264 -preset veryfast -b:v " ++ show bv ++ "k " ++
                    " -c:a libfdk_aac -b:a " ++ show ba ++ "k " ++
                    " -f matroska"

                _       -> -- webm
                    " -c:v vp8 -quality realtime -cpu-used 10 -threads 4 -slices 4 -b:v " ++ show bv ++ "k" ++
                    " -c:a libvorbis  -b:a " ++ show ba ++ "k" ++
                    " -f webm"

            cmd = input ++ transcode ++ " -"

            str write flush = do
                void $ sourceCmdWithConsumer cmd $ awaitForever $ \bs -> lift $ write (fromByteString bs) >> flush
                flush

        liftIO $ putStrLn cmd
        return $ responseStream status200 [ ("Content-Type", "video/webm") ] str

streamDirect :: (MonadMask m, MonadIO m) => FileId -> Authenticated m Response
streamDirect = withFileAccess $ \f mime ->
    return $ responseFile status200 [("Content-Type", encodeUtf8 mime)] f Nothing
