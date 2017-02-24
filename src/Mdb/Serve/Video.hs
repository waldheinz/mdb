
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mdb.Serve.Video (
    videoApp
    ) where

import           Blaze.ByteString.Builder   (Builder, fromByteString)
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class ( asks )
import qualified Data.Conduit.List as CL
import           Data.Conduit.Process
import           Data.Fixed (divMod')
import           Data.Monoid                ((<>))
import qualified Data.Text as T
import           Data.Text.Encoding         (encodeUtf8)
import           Network.HTTP.Types         (status200)
import           Network.Wai
import           Network.Wai.Predicate
import           Network.Wai.Routing
import           System.FilePath ( (</>) )

import           Mdb.Database
import           Mdb.Image                  (ensureFrame )
import           Mdb.Serve.Auth             (Authenticated)
import qualified Mdb.Serve.Auth             as AUTH
import           Mdb.Serve.Utils            (withFileAccess)
import qualified Mdb.Serve.TranscodeSpec    as SPEC
import           Mdb.Types

videoApp :: MediaDb -> AUTH.SessionKey IO -> Application
videoApp mdb skey req respond = runMDB' mdb $ route root req (liftIO . respond) where
    goAuth = AUTH.request skey req
    root = prepare $ do
        get "/:id/dash/:file"   (continue $ goAuth . streamDash)    $ capture "id" .&. capture "file"
        get "/:id/frame"        (continue $ goAuth . frame)         $ capture "id" .&. def 0 (query "ts")
        get "/:id/variants"     (continue $ goAuth . variants)      $ capture "id"
        get "/:id/stream"       (continue $ goAuth . stream)
            $   capture "id"
            .&. def 0 (query "t")
            .&. opt (query "end")
            .&. def 720 (query "rv")
            .&. def 2000 (query "maxrate")
            .&. def 4000 (query "bufsize")
            .&. def 28 (query "crf")
            .&. def 96 (query "ba")
            .&. def "mkv" (query "format")
        get "/:id/hls"          (continue $ goAuth . hls)
            $ capture "id" .&. def 480 (query "rv") .&. def 2000 (query "bv") .&. def 64 (query "ba")
        get "/:id/streamDirect" (continue $ goAuth . streamDirect) $ capture "id"
        get "/:id/transcode" ( continue $ goAuth . transcoded) $ capture "id" .&. query "spec"

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
    :: (MonadMask m, MonadIO m) => (FileId ::: Double ::: Maybe Double ::: Int ::: Int ::: Int ::: Int ::: Int ::: String)
    -> Authenticated m Response
stream (fid ::: start ::: end ::: rv ::: maxrate ::: bufsize ::: crf ::: ba ::: fmt) = withFileAccess go fid where
    go p _ = do
        let
            input =
                "ffmpeg -nostdin -loglevel quiet -copyts" ++
                " -ss " ++ show start ++
                " -i \"" ++ p ++ "\"" ++
                maybe "" (\l -> " -to " ++ show l) end ++
                " -vf scale=-2:" ++ show rv

            transcode = case fmt of
                "mkv"   ->
                    " -c:v libx264 -preset veryfast" ++
                    " -maxrate " ++ show maxrate ++ "k" ++
                    " -bufsize " ++ show bufsize ++ "k" ++
                    " -crf " ++ show crf ++
                    " -c:a libfdk_aac -b:a " ++ show ba ++ "k " ++
                    " -f matroska"

                _       -> -- webm
                    " -c:v vp8 -quality realtime -cpu-used 10 -threads 4 -slices 4" ++
                    " -maxrate " ++ show maxrate ++ "k" ++
                    " -bufsize " ++ show bufsize ++ "k" ++
                    " -c:a libvorbis  -b:a " ++ show ba ++ "k" ++
                    " -f webm"

            cmd = input ++ transcode ++ " -"

            str write flush = do
                void $ sourceCmdWithConsumer cmd (CL.mapM_ $ \bs -> write (fromByteString bs))
                flush

        liftIO $ putStrLn cmd
        return $ responseStream status200 [ ("Content-Type", "video/webm") ] str

streamDirect :: (MonadMask m, MonadIO m) => FileId -> Authenticated m Response
streamDirect = withFileAccess $ \f mime ->
    return $ responseFile status200 [("Content-Type", encodeUtf8 mime)] f Nothing

streamDash :: (MonadMask m, MonadIO m) => (FileId ::: FilePath) -> Authenticated m Response
streamDash (fid ::: fname) = withFileAccess go fid where
    go _ _ = do
        base <- AUTH.unsafe $ asks mdbDbDir
        return $ responseFile status200 [] (base </> "dash" </> show fid </> fname) Nothing

------------------------------------------------------------------------------------------------------------------------
-- Transcoding
------------------------------------------------------------------------------------------------------------------------

transcoded :: (MonadMask m, MonadIO m) => FileId ::: SPEC.TranscodeSpec -> Authenticated m Response
transcoded (fid ::: spec) = withFileAccess doit fid where
    doit file _ = do
        let
            streams = SPEC.streams spec
            idxStreams = zip ([0..] :: [Int]) $ map (\(SPEC.Stream _ x) -> x) streams
            streamMap = concatMap go streams where
                go (SPEC.Stream src _ ) = "-map 0:" ++ show src ++ " "

            streamOpts = concatMap go idxStreams where
                go (i, SPEC.CopyVideo)  = "-c:v:" ++ show i ++ " copy "
                go (i, SPEC.CopyAudio)  = "-c:a:" ++ show i ++ " copy "
                go (i, SPEC.TranscodeH264 (SPEC.VideoSpecH264 mr bs crf w)) =
                    " -vf scale=-2:" ++ show w ++
                    " -c:v:" ++ show i ++
                    " libx264 -preset superfast" ++
                    " -maxrate " ++ show mr ++ "k" ++
                    " -bufsize " ++ show bs ++ "k" ++
                    " -crf " ++ show crf ++ " "

                go (i, SPEC.TranscodeAAC (SPEC.AudioSpecAAC br)) = "-c:a:" ++ show i ++
                    " libfdk_aac -b:a " ++ show br ++ "k "

            (format, mime) = case SPEC.container spec of
                SPEC.Matroska   -> ("-f matroska", "video/matroska")
                SPEC.MP4        -> ("-f mp4 -movflags frag_keyframe+empty_moov", "video/mp4")

            prefix = "ffmpeg -nostdin -loglevel quiet -copyts "
            input = " -i \"" ++ file ++ "\" "
            cmd = prefix ++ input ++ streamMap ++ streamOpts ++ format ++ " -"

        liftIO $ putStrLn $ show spec
        liftIO $ putStrLn cmd

        return $ responseStream status200 [ ("Content-Type", mime) ] $ \write flush -> do
            void $ sourceCmdWithConsumer cmd (CL.mapM_ $ \bs -> write (fromByteString bs))
            flush
