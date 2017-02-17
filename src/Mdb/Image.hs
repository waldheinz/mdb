
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Image (
    ThumbSize(..), ensureThumb, ensureFrame, ensureThumbs
    ) where

import           Control.Applicative             ( (<|>) )
import           Control.Monad                   (unless, forM, forM_)
import           Control.Monad.Catch             (MonadMask)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader.Class      (asks)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Except
import           Data.Attoparsec.ByteString      ( string )
import qualified Data.ByteString.Conversion.From as BSCL
import qualified Data.ByteString.Lazy            as BSL
import           Data.Digest.Pure.MD5            (md5)
import           Data.Maybe                      (catMaybes)
import           Data.String                     (fromString)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (encodeUtf8)
import qualified Graphics.ImageMagick.MagickWand as IM
import           System.FilePath                 (takeDirectory)
import           System.Directory                (createDirectoryIfMissing,
                                                  doesFileExist)
import           System.Process                  (callCommand)

import           Mdb.Database
import           Mdb.Types

data ThumbSize
    = Small
    | Medium
    | Large
    deriving ( Show )

instance BSCL.FromByteString ThumbSize where
    parser =
        (string "small"     >> return Small) <|>
        (string "medium"    >> return Medium) <|>
        (string "large"     >> return Large)

thumbDir :: ThumbSize -> String
thumbDir Small  = "small"
thumbDir Medium = "medium"
thumbDir Large  = "large"

thumbPixels :: ThumbSize -> Int
thumbPixels Small   = 128
thumbPixels Medium  = 256
thumbPixels Large   = 512

ensureThumb
    :: (MonadMask m, MonadIO m)
    => ThumbSize -> FileId -> FilePath -> T.Text -> ExceptT T.Text (MDB m) FilePath
ensureThumb ts fid filePath fileMime = do
    srcFile <- thumbSource fid filePath fileMime
    lift $ missingThumbs srcFile [ts] >>= genThumbs srcFile
    lift $ thumbFileName ts srcFile

ensureThumbs :: (MonadMask m, MonadIO m) => FileId -> FilePath -> T.Text -> ExceptT T.Text (MDB m) ()
ensureThumbs fid filePath fileMime = do
    src <- thumbSource fid filePath fileMime
    lift $ missingThumbs src [Small, Medium, Large] >>= genThumbs src

thumbSource
    :: (MonadIO m, MonadMask m)
    => FileId -> [Char] -> T.Text -> ExceptT T.Text (MDB m) [Char]
thumbSource fid filePath fileMime = case T.takeWhile ( /= '/') fileMime of
    "image" -> return filePath
    "video" -> lift $ ensureFrame filePath fid 180
    _       -> throwE "unknown mime type for thumb generation"

thumbFileName :: MonadIO m => ThumbSize -> FilePath -> MDB m FilePath
thumbFileName ts src = do
    dbDir   <- asks mdbDbDir
    relSrc  <- relFile src

    let
        thumbsBaseDir       = dbDir ++ "/thumbs/" ++ thumbDir ts ++ "/"
        hashString          = show (md5 $ BSL.fromStrict $ encodeUtf8 $ T.pack relSrc)
        (hashPath, fname)   = splitAt 2 hashString
        dir                 = thumbsBaseDir ++ hashPath ++ "/"

    return $ dir ++ fname ++ ".jpg"

missingThumbs :: MonadIO m => FilePath -> [ThumbSize] -> MDB m [(ThumbSize, FilePath)]
missingThumbs fn tss = catMaybes <$> forM tss go
    where
        go ts = do
            tn      <- thumbFileName ts fn
            exists  <- liftIO $ doesFileExist tn

            return $ if not exists
                then Just (ts, tn)
                else Nothing

genThumbs :: MonadIO m => FilePath -> [(ThumbSize, FilePath)] -> MDB m ()
genThumbs _ [] = return ()
genThumbs src missing = liftIO $ IM.localGenesis $ do
    (_,wand) <- IM.magickWand
    IM.readImage wand $ fromString src
    w <- IM.getImageWidth wand
    h <- IM.getImageHeight wand
    forM_ missing $ \(ts, dst) -> do
        lift $ createDirectoryIfMissing True $ takeDirectory dst

        let
            sz = thumbPixels ts
            (w', h') = if w > h
                       then ( sz, floor $ fromIntegral sz * (fromIntegral h / (fromIntegral w :: Float)) )
                       else ( floor $ fromIntegral sz * (fromIntegral w / (fromIntegral h :: Float)), sz )

        IM.resizeImage wand w' h' IM.lanczosFilter 1
        IM.writeImages wand (fromString dst) True
        lift $ putStrLn $ "generated " ++ show ts ++ " for " ++ src

roundTimeToMs :: Double -> Integer
roundTimeToMs ts = round ts `div` 30 * 30000

ensureFrame :: (MonadMask m, MonadIO m) => FilePath -> FileId -> Double -> MDB m FilePath
ensureFrame p fid ts = do
    dbDir <- asks mdbDbDir

    let
        frameDir    = dbDir ++ "/videoFrames/"
        tsMs        = roundTimeToMs ts
        tsS         = fromIntegral tsMs / 1000 :: Double
        outFile     = frameDir ++ "/frame-" ++ show fid ++ "ts" ++ show tsMs ++ ".jpg"
        cmd         =
            "ffmpeg -y -ss " ++ show tsS ++ " -i \"" ++ p ++ "\" -t 1 -f image2 -update 1 \"" ++ outFile ++ "\""

    exists <- liftIO $ doesFileExist outFile
    unless exists $ liftIO $ createDirectoryIfMissing True frameDir >> callCommand cmd
    return outFile
