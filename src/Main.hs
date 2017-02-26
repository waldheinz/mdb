
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
  ) where

import           Codec.FFmpeg ( initFFmpeg )
import           Control.Exception ( bracket_ )
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Control.Monad.Logger   as LOG
import qualified Crypto.Scrypt as SCRYPT
import qualified Data.ByteString as BS
import           System.Directory ( getCurrentDirectory )
import           Graphics.ImageMagick.MagickWand ( withMagickWandGenesis )
import           System.IO

import qualified Mdb.CmdLine  as CMD
import qualified Mdb.Database as DB
import qualified Mdb.Serve as SERVE
import qualified Mdb.TvShow as TV
import           Mdb.File
import           Mdb.Album ( doAlbum )

main :: IO ()
main = withMagickWandGenesis $ liftIO $ do
    initFFmpeg
    opts <- CMD.parseCommandLine

    let
        mroot = CMD.rootDir opts

    LOG.runStderrLoggingT $ case CMD.mode opts of
        CMD.ModeAlbum opt           -> DB.findDbAndRun mroot $ doAlbum opt
        CMD.ModeFile op rec fs      -> DB.findDbAndRun mroot $ doFile op rec fs
        CMD.ModePerson op           -> DB.findDbAndRun mroot $ doPerson op
        CMD.ModeInit                -> doInit mroot
        CMD.ModeTvShow op           -> DB.findDbAndRun mroot $ TV.doMode op
        CMD.ModeServe               -> DB.findDbAndRun mroot SERVE.doServe
        CMD.ModeUser (CMD.AddUser n)-> DB.findDbAndRun mroot $ doAddUser n

doAddUser :: (MonadMask m, MonadIO m) => String -> DB.MDB m ()
doAddUser name = do
    let
        withEcho :: Bool -> IO a -> IO a
        withEcho echo action = do
            old <- hGetEcho stdin
            bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

    p <- liftIO $ do
        putStr $ "Adding user " ++ name ++ ", please give password: "
        hFlush stdout
        pass <- withEcho False BS.getLine
        putChar '\n'
        return pass

    hashed <- liftIO $ SCRYPT.encryptPassIO' (SCRYPT.Pass p)
    DB.dbExecute "INSERT INTO user(user_name, user_pass_scrypt) VALUES (?, ?)" (name, SCRYPT.getEncryptedPass hashed)
    DB.dbLastRowId >>= \uid -> liftIO $ putStrLn $ "User " ++ name ++ " added with id " ++ show uid

doPerson :: (MonadMask m, MonadIO m) => CMD.OptPerson -> DB.MDB m ()
doPerson (CMD.AddPerson n) = DB.addPerson n >>= \pid ->
    liftIO $ putStrLn $ "added \"" ++ n ++ "\" with ID " ++ show pid
doPerson (CMD.SetPersonPortrait pid fid) = DB.dbExecute
    "UPDATE person SET person_portrait = ? WHERE person_id = ?"
    (fid, pid)

doInit :: (LOG.MonadLogger m, MonadIO m) => Maybe FilePath -> m ()
doInit mp = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> liftIO $ getCurrentDirectory

  DB.initDb p
