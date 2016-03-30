
{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Mdb.Serve.VideoWs (
    videoWsApp
    ) where

import qualified Codec.FFmpeg.Decode as FFM
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Except
import qualified Data.Text as T
import qualified Network.WebSockets as WS

videoWsApp :: WS.ServerApp
videoWsApp pending = WS.acceptRequest pending >>= runConnection

runConnection conn = do
    WS.forkPingThread conn 10
    result <- runExceptT $ handshake conn

    case result of
        Left e -> print e
        Right () -> return ()

handshake :: (MonadIO m, MonadError String m) => WS.Connection -> m ()
handshake conn = do
    cookie <- readText conn
    fileId <- readText conn
    ictx <- FFM.openFile "test/SMBD-110.m2ts.h265.mkv"
    
    return ()

readText conn = liftIO (WS.receiveDataMessage conn) >>= \msg -> case msg of
    WS.Binary  _    -> throwError "expected text handshake"
    WS.Text txt     -> liftIO $ print txt
