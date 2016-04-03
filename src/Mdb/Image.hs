
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Image (
    ensureThumb, ensureFrame
    ) where

import           Control.Monad                   (unless)
import           Control.Monad.Catch             (MonadMask)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader.Class      (asks)
import qualified Data.ByteString.Lazy            as BSL
import           Data.Digest.Pure.MD5            (md5)
import           Data.String                     (fromString)
import qualified Data.Text                       as T
import           Data.Text.Encoding              (encodeUtf8)
import qualified Graphics.ImageMagick.MagickWand as IM
import           System.Directory                (createDirectoryIfMissing,
                                                  doesFileExist)
import           System.Process                  (callCommand)

import           Mdb.Database
import           Mdb.Types

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


ensureThumb :: (MonadMask m, MonadIO m) => FileId -> FilePath -> T.Text -> MDB m FilePath
ensureThumb fid filePath fileMime = do
    srcFile <- case T.takeWhile ( /= '/') fileMime of
        "image" -> return filePath
        "video" -> ensureFrame filePath fid 180
        _       -> fail "fileThumb unknown type"

    ensureImageThumb srcFile

ensureImageThumb :: MonadIO m => FilePath -> MDB m FilePath
ensureImageThumb src = do
    dbDir <- asks mdbDbDir

    let
        thumbDir    = dbDir ++ "/thumbs/normal/"
        thumbFile   = thumbDir ++ show (md5 $ BSL.fromStrict $ encodeUtf8 $ T.pack src) ++ ".jpg"

    exists <- liftIO $ doesFileExist thumbFile
    unless exists $ do

        liftIO $ createDirectoryIfMissing True thumbDir
        liftIO $ IM.localGenesis $ do
            (_,wand) <- IM.magickWand
            IM.readImage wand $ fromString src
            w <- IM.getImageWidth wand
            h <- IM.getImageHeight wand

            let
                sz = 256
                (w', h') = if w > h
                           then ( sz, floor $ fromIntegral sz * (fromIntegral h / (fromIntegral w :: Float)) )
                           else ( floor $ fromIntegral sz * (fromIntegral w / (fromIntegral h :: Float)), sz )

            IM.resizeImage wand w' h' IM.lanczosFilter 1
            IM.writeImages wand (fromString thumbFile) True

    return thumbFile
