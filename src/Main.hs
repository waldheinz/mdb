
module Main (
  main
  ) where

import Codec.FFmpeg ( initFFmpeg )
import Control.Monad.IO.Class ( liftIO )
import System.Directory ( getCurrentDirectory, copyFile )
import System.Environment ( getArgs )


import qualified CmdLine  as CMD
import qualified Database as DB
import qualified Serve    as SERVE
import Mdb.File


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
            CMD.ModeServe       -> DB.findDbAndRun $ SERVE.doServe

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
