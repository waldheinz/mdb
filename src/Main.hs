
module Main (
  main
  ) where

import qualified CmdLine  as CMD
import qualified Database as DB
import qualified Scan     as SCAN

import Codec.FFmpeg ( initFFmpeg )
import Control.Monad.Catch ( MonadMask )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

main :: IO ()
main = do
    initFFmpeg
    argv <- getArgs

    case CMD.parseMode argv of
        Left x     -> putStrLn $ x
        Right mode -> case mode of
            CMD.ModeInit oi -> doInit oi
            CMD.ModeScan os -> findDbAndRun $ SCAN.doScan os

doInit :: CMD.OptInit -> IO ()
doInit (CMD.OptInit mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory

  DB.initDb p

findDbAndRun :: (MonadMask m, MonadIO m) => DB.MDB m () -> m ()
findDbAndRun act = liftIO DB.findDbFolder >>= \x -> case x of
  Nothing  -> liftIO $ putStrLn $ "no db directory found, maybe try \"mdb init\"?"
  Just dbf -> DB.runMDB dbf act