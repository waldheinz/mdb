
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Status ( doStatus ) where

import           Control.Concurrent.Async ( async, wait, waitAny )
import           Control.Monad ( unless )
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Logger ( logInfoN, logWarnN, logDebugN )
import           Control.Monad.Reader ( ask )
import qualified Database.SQLite.Simple as SQL
import           Data.List ( delete )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           GHC.Conc ( getNumCapabilities )
import           System.IO.Error ( tryIOError )
import           System.Posix.Files ( getFileStatus, modificationTimeHiRes )

import           Mdb.Database ( MDB, dbQuery, runMDB' )
import           Mdb.Types ( FileId )

doStatus :: (MonadMask m, MonadIO m) => MDB m ()
doStatus = withFiles (mapM_ checkFile)

type FileInfo = (FileId, FilePath)

withFiles :: (MonadIO m, MonadMask m) => ([FileInfo] -> MDB IO ()) -> MDB m ()
withFiles f = go (0 :: Int) [] where
    go offset as = do
        fs <- dbQuery "SELECT file_id, file_name FROM file LIMIT 100 OFFSET ?" (SQL.Only offset)
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
                go (offset + 100) (a : as')

checkFile :: MonadIO m => FileInfo -> MDB m ()
checkFile (fid, fp) = do
    efs <- liftIO $ tryIOError $ getFileStatus fp
    case efs of
        Left ioe    -> logWarnN $ T.pack ( show ioe )
        Right fs    -> logDebugN $ T.pack (show $ modificationTimeHiRes fs)
