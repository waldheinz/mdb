
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Status ( doStatus ) where

import           Control.Concurrent.Async ( async, wait, waitAny )
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Logger ( logWarnN, logDebugN )
import           Control.Monad.Reader ( ask )
import qualified Database.SQLite.Simple as SQL
import           Data.List ( delete )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           GHC.Conc ( getNumCapabilities )
import           System.IO.Error ( tryIOError )
import           System.Posix.Files ( getFileStatus, modificationTimeHiRes )

import           Mdb.Database ( MDB, dbQuery, runMDB', withConnection )
import           Mdb.Types ( FileId )

doStatus :: (MonadMask m, MonadIO m) => MDB m ()
doStatus = withFilesSeq checkFile

type FileInfo = (FileId, FilePath)

withFilesSeq :: (MonadIO m, MonadMask m) => (FileInfo -> MDB IO ()) -> MDB m ()
withFilesSeq f = withConnection $ \c -> do
    mdb <- ask
    liftIO $ SQL.withStatement c "SELECT file_id, file_name FROM file ORDER BY file_id" $ \stmt -> do

        let
            go = SQL.nextRow stmt >>= \mfi -> case mfi of
                Nothing -> return ()
                Just fi -> (runMDB' mdb $ f fi) >> go

        go

batchSize :: Int
batchSize = 1000

withFilesPar :: (MonadIO m, MonadMask m) => ([FileInfo] -> MDB IO ()) -> MDB m ()
withFilesPar f = go (0 :: Int) [] where
    go offset as = do
        fs <- dbQuery "SELECT file_id, file_name FROM file ORDER BY file_id LIMIT ? OFFSET ?" (batchSize, offset)
        if (null fs)
            then do
                logDebugN $ "waiting for " <> (T.pack $ show $ length as) <> " tasks"
                liftIO $ mapM_ wait as
            else do
                numCap <- liftIO $ getNumCapabilities
                as' <- if length as < numCap
                    then return as
                    else do
                        (x, ()) <- liftIO $ waitAny as
                        return $ delete x as

                mdb <- ask
                a <- liftIO $ async $ (runMDB' mdb $ f fs)
                go (offset + batchSize) (a : as')

checkFile :: MonadIO m => FileInfo -> MDB m ()
checkFile (_, fp) = do
    efs <- liftIO $ tryIOError $ getFileStatus fp
    case efs of
        Left ioe    -> logWarnN $ T.pack ( show ioe )
        Right fs    -> logDebugN $ T.pack (show $ modificationTimeHiRes fs)
