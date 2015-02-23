
module Main (
  main
  ) where

import qualified CmdLine  as CMD
import qualified Database as DB
import qualified Scan     as SCAN
import qualified Serve    as SERVE

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
            CMD.ModeScan os -> DB.findDbAndRun $ SCAN.doScan os
            CMD.ModeServe   -> DB.findDbAndRun $ SERVE.doServe

doInit :: CMD.OptInit -> IO ()
doInit (CMD.OptInit mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory

  DB.initDb p

