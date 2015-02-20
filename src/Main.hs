
module Main (
  main
  ) where

import qualified CmdLine  as CMD
import qualified Database as DB
import qualified Scan     as SCAN

import Codec.FFmpeg ( initFFmpeg )
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
            CMD.ModeScan os -> SCAN.doScan os

doInit :: CMD.OptInit -> IO ()
doInit (CMD.OptInit mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory

  DB.initDb p

