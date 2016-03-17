
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Thumbs (
    thumbApp
    ) where


import           Control.Monad ( unless )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader.Class ( asks )
import qualified Data.ByteString.Lazy as BSL
import           Data.Digest.Pure.MD5 ( md5 )
import           Data.String ( fromString )
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Graphics.ImageMagick.MagickWand as IM
import           Network.HTTP.Types ( status200 )
import           Network.Wai
import           Network.Wai.Routing
import           System.Directory ( doesFileExist, createDirectoryIfMissing )

import           Mdb.Database
import qualified Mdb.Database.File as DBF
import qualified Mdb.Serve.Video as V

thumbApp :: MediaDb -> Application
thumbApp mdb req respond = runMDB' mdb $ route root req (liftIO . respond) where
    root = prepare $
        get "/medium/:fid"        (continue fileThumb)        $ capture "fid"

fileThumb :: MonadIO m => DBF.FileId -> MDB m Response
fileThumb fid = do
    file <- fileById fid
    srcFile <- case T.takeWhile ( /= '/') (DBF.fileMime file) of
        "image" -> fileAbs $ DBF.filePath file
        "video" -> V.ensureFrame fid 180
        _       -> fail "fileThumb unknown type"

    thumbFile <- ensureThumb srcFile
    
    return $ responseFile status200
        [ ("Cache-Control", "max-age=3600")
        , ("Content-Type", "image/jpeg")
        ] thumbFile Nothing

ensureThumb :: MonadIO m => FilePath -> MDB m FilePath
ensureThumb src = do
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
            liftIO $ putStrLn "gelesen"
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
