
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Image (
    imageApp
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

import           Database
import qualified Mdb.Database.File as DBF

imageApp :: MediaDb -> Application
imageApp mdb req respond = runMDB' mdb
    $ route start req (liftIO . respond)

start :: MonadIO m => Tree (App (MDB m))
start = prepare $ do
    get "/image/:id" (continue getImage)        $ capture "id"
    get "/person/:id" (continue pImage)         $ capture "id"
    get "/thumbnail/:fid" (continue fileThumb)  $ capture "fid"

getImage :: MonadIO m => DBF.FileId -> MDB m Response
getImage fid = do
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f
    return $ responseFile status200 [] p Nothing

pImage :: MonadIO m => Integer -> MDB m Response
pImage pid = personImageFile pid >>= ensureThumb >>=
    \p -> return $ responseFile status200 [] p Nothing

fileThumb :: MonadIO m => DBF.FileId -> MDB m Response
fileThumb fid = fileById fid >>= \file -> case T.takeWhile ( /= '/') (DBF.fileMime file) of
    "image" -> imageThumb file
    _       -> fail "fileThumb unknown type"

imageThumb :: MonadIO m => DBF.File -> MDB m Response
imageThumb file = do
    thumbFile <- ensureThumb $ DBF.filePath file
    return $ responseFile status200 [] thumbFile Nothing

ensureThumb :: MonadIO m => FilePath -> MDB m FilePath
ensureThumb relPath = do
    dbDir <- asks mdbDbDir

    let
        thumbDir    = dbDir ++ "/thumbs/normal/"
        thumbFile   = thumbDir ++ (show $ md5 $ BSL.fromStrict $ encodeUtf8 $ T.pack relPath) ++ ".png"

    exists <- liftIO $ doesFileExist thumbFile

    unless exists $ do
        src <- fileAbs relPath
        liftIO $ createDirectoryIfMissing True thumbDir
        liftIO $ IM.localGenesis $ do
            (_,wand) <- IM.magickWand
            IM.readImage wand $ fromString src
            w <- IM.getImageWidth wand
            h <- IM.getImageHeight wand

            let
                sz = 256
                (w', h') = if w > h
                           then ( sz, floor $ (fromIntegral sz) * (fromIntegral h / (fromIntegral w :: Float)) )
                           else ( floor $ (fromIntegral sz) * (fromIntegral w / (fromIntegral h :: Float)), sz )

            IM.resizeImage wand w' h' IM.lanczosFilter 1
            IM.writeImages wand (fromString thumbFile) True

    return thumbFile
