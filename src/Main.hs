
module Main (
  main
  ) where

import           Codec.FFmpeg ( initFFmpeg )
import           Control.Monad.IO.Class ( liftIO )
import           System.Directory ( getCurrentDirectory, copyFile )
import           Graphics.ImageMagick.MagickWand ( withMagickWandGenesis )

import qualified Mdb.CmdLine  as CMD
import qualified Database as DB
import qualified Serve    as SERVE
import           Mdb.File
import           Mdb.Album ( doAlbum )

main :: IO ()
main = withMagickWandGenesis $ liftIO $ do
    initFFmpeg
    mode <- CMD.parseCommandLine

    case mode of
        CMD.ModeAlbum opt           -> DB.findDbAndRun $ doAlbum opt
        (CMD.ModeFile op rec fs)    -> DB.findDbAndRun $ doFile op rec fs
        CMD.ModePerson op           -> DB.findDbAndRun $ doPerson op
        CMD.ModeInit oi             -> doInit oi
        CMD.ModeServe               -> DB.findDbAndRun $ SERVE.doServe

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
