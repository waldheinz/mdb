
module Main (
  main
  ) where

import qualified CmdLine  as CMD
import qualified Database as DB
import qualified Scan     as SCAN
import qualified Serve    as SERVE

import Codec.FFmpeg ( initFFmpeg )
import Control.Monad.IO.Class ( liftIO )
import System.Directory ( getCurrentDirectory, copyFile )
import System.Environment ( getArgs )

main :: IO ()
main = do
    initFFmpeg
    argv <- getArgs

    case CMD.parseMode argv of
        Left x     -> putStrLn $ x
        Right mode -> case mode of
            CMD.ModeFile o      -> DB.findDbAndRun $ doFile o
            CMD.ModePerson op   -> DB.findDbAndRun $ doPerson op
            CMD.ModeInit oi     -> doInit oi
            CMD.ModeScan os     -> DB.findDbAndRun $ SCAN.doScan os
            CMD.ModeServe       -> DB.findDbAndRun $ SERVE.doServe

doFile :: CMD.OptFile -> DB.MDB IO ()
doFile (CMD.FileAssignPerson pid files) = mapM_ go files where
    go file = do
        mfid <- DB.fileIdFromName file
        case mfid of
             Just fid   -> DB.assignFilePerson fid pid
             Nothing    -> fail $ file ++ " not registered yet"

doPerson :: CMD.OptPerson -> DB.MDB IO ()
doPerson (CMD.AddPerson n) = (DB.addPerson n) >>= \pid ->
    liftIO $ putStrLn $ "added \"" ++ n ++ "\" with ID " ++ show pid
doPerson (CMD.SetPersonImage pid file) = DB.personImageFile pid >>= (liftIO . copyFile file)

doInit :: CMD.OptInit -> IO ()
doInit (CMD.OptInit mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory

  DB.initDb p
