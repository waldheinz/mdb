
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Video (
    videoApp
    ) where

import           Blaze.ByteString.Builder ( fromByteString )
import Blaze.ByteString.Builder.Char8 ( fromString )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Trans.Class ( lift )
import qualified Data.ByteString as BS
import           Data.Conduit
import           Data.Conduit.Process
import           Network.HTTP.Types ( status200 )
import           Network.Wai
import           Network.Wai.Routing

import           Database
import qualified Mdb.Database.File as DBF

videoApp :: MediaDb -> Application
videoApp mdb req respond = runMDB' mdb $ route root req (liftIO . respond) where
    root = prepare $ do
        get "/stream/:id"       (continue stream)       $ capture "id"
        get "/streamDirect/:id" (continue streamDirect) $ capture "id"

streamDirect :: MonadIO m => DBF.FileId -> MDB m Response
streamDirect fid = do
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f
    return $ responseFile status200 [] p Nothing

stream :: MonadIO m => DBF.FileId -> MDB m Response
stream fid = do
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f

    let
        cmd = "ffmpeg " ++
            "-i \"" ++ p ++ "\" " ++
            "-f matroska " ++
            " - 2>/dev/null" :: String

        str write flush = do
            sourceCmdWithConsumer cmd $ do
                awaitForever $ \bs -> do
                    lift $ write (fromByteString bs)
                    lift $ putStrLn $ show (BS.length bs, " bytes written")
                    -- lift $ flush
            flush

    liftIO $ putStrLn $ "command is " ++ cmd
    return $ responseStream status200 [] str
