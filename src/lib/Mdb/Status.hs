
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Status ( doStatus ) where

import           Control.Monad ( when )
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Logger ( logWarnN, logDebugN, logInfoN )
import           Control.Monad.Reader ( ask )
import qualified Database.SQLite.Simple as SQL
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           System.IO.Error ( tryIOError )
import           System.Posix.Files ( getFileStatus, modificationTimeHiRes )

import           Mdb.CmdLine ( OptStatus(..) )
import           Mdb.Database ( MDB, dbExecute, runMDB', withConnection )
import           Mdb.Types ( FileId )

doStatus :: (MonadMask m, MonadIO m) => OptStatus -> MDB m ()
doStatus = withFilesSeq . checkFile

type FileInfo = (FileId, FilePath)

withFilesSeq :: (MonadIO m, MonadMask m) => (FileInfo -> MDB IO ()) -> MDB m ()
withFilesSeq f = withConnection $ \c -> do
    mdb <- ask
    liftIO $ SQL.withStatement c "SELECT file_id, file_name FROM file ORDER BY file_id" $ \stmt ->
        let
            go = SQL.nextRow stmt >>= \mfi -> case mfi of
                Nothing -> return ()
                Just fi -> (runMDB' mdb $ f fi) >> go

        in go

checkFile :: (MonadIO m, MonadMask m) => OptStatus -> FileInfo -> MDB m ()
checkFile op (fid, fp) = do
    efs <- liftIO $ tryIOError $ getFileStatus fp
    case efs of
        Left ioe    -> do
            logWarnN $ T.pack ( show ioe )
            when (removeMissing op) $ do
                logInfoN $ "removing file with ID " <> (T.pack $ show fid)
                dbExecute "DELETE FROM file WHERE file_id = ?" (SQL.Only fid)
        Right fs    -> logDebugN $ T.pack (show $ modificationTimeHiRes fs)
