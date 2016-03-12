
module Main (
  main
  ) where

import           Codec.FFmpeg ( initFFmpeg )
import           Control.Monad.IO.Class ( liftIO )
import           System.Directory ( getCurrentDirectory, copyFile )
import           Graphics.ImageMagick.MagickWand ( withMagickWandGenesis )

import qualified Mdb.CmdLine  as CMD
import qualified Mdb.Database as DB
import qualified Mdb.Serve as SERVE
import           Mdb.File
import           Mdb.Album ( doAlbum )

main :: IO ()
main = withMagickWandGenesis $ liftIO $ do
    initFFmpeg
    opts <- CMD.parseCommandLine

    let
        mroot = CMD.rootDir opts

    case CMD.mode opts of
        CMD.ModeAlbum opt           -> DB.findDbAndRun mroot $ doAlbum opt
        CMD.ModeFile op rec fs      -> DB.findDbAndRun mroot $ doFile op rec fs
        CMD.ModePerson op           -> DB.findDbAndRun mroot $ doPerson op
        CMD.ModeInit                -> doInit mroot
        CMD.ModeServe               -> DB.findDbAndRun mroot SERVE.doServe

doPerson :: CMD.OptPerson -> DB.MDB IO ()
doPerson (CMD.AddPerson n) = DB.addPerson n >>= \pid ->
    liftIO $ putStrLn $ "added \"" ++ n ++ "\" with ID " ++ show pid
doPerson (CMD.SetPersonImage pid file) = DB.personImageFile pid >>= (liftIO . copyFile file)

doInit :: Maybe FilePath -> IO ()
doInit mp = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory

  DB.initDb p
