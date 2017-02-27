
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Status ( doStatus ) where

import           Control.Monad ( unless )
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Logger ( logInfoN, logWarnN, logDebugN )
import qualified Database.SQLite.Simple as SQL
import qualified Data.Text as T
import           System.IO.Error ( tryIOError )
import           System.Posix.Files ( getFileStatus, modificationTimeHiRes )

import           Mdb.Database ( MDB, dbQuery )
import           Mdb.Types ( FileId )

doStatus :: (MonadMask m, MonadIO m) => MDB m ()
doStatus = withFiles (mapM_ checkFile)

type FileInfo = (FileId, FilePath)

withFiles :: (MonadIO m, MonadMask m) => ([FileInfo] -> MDB m ()) -> MDB m ()
withFiles f = go (0 :: Int) where
    go offset = do
        fs <- dbQuery "SELECT file_id, file_name FROM file LIMIT 100 OFFSET ?" (SQL.Only offset)
        unless (null fs) $ f fs >> go (offset + 100)

checkFile :: MonadIO m => FileInfo -> MDB m ()
checkFile (fid, fp) = do
    efs <- liftIO $ tryIOError $ getFileStatus fp
    case efs of
        Left ioe    -> logWarnN $ T.pack ( show ioe )
        Right fs    -> logDebugN $ T.pack (show $ modificationTimeHiRes fs)
