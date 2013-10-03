
module Scan (
  doScan
  ) where

import Control.Monad ( forM )
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.Pure.SHA ( bytestringDigest, sha1 )
import Data.Maybe ( catMaybes )
import System.Directory ( doesDirectoryExist, getCurrentDirectory, getDirectoryContents )
import System.FilePath ( (</>) )
import System.IO

import qualified CmdLine  as CMD
import qualified Database as DB

doScan :: CMD.OptScan -> IO ()
doScan CMD.OptScan = DB.findDbFolder >>= \x -> case x of
  Nothing  -> putStrLn $ "no db directory found, maybe try \"mdb init\"?"
  Just dbf -> DB.withDbFolder dbf $ \db -> do
    here <- getCurrentDirectory
    traverseFiles here $ checkFile db

checkFile :: DB.MediaDb -> FilePath -> IO ()
checkFile db fn = do
  putStrLn fn

  withFile fn ReadMode $ \h -> do
    contents <- BSL.hGetContents h
    size <- hFileSize h
    DB.addFile db (fn, size, BSL.toStrict $ bytestringDigest $ sha1 contents)

  return ()

traverseFiles :: FilePath -> (FilePath -> IO ()) -> IO ()
traverseFiles fp act = do
  cs <- getDirectoryContents fp
  subs <- forM cs $ \c -> do
    doesDirectoryExist (fp </> c) >>= \dir ->
      if dir
      then return $ Just c
      else act (fp </> c) >> return Nothing

  mapM_ (\sub -> traverseFiles (fp </> sub) act) $
    filter (\d -> (d /= "." && d /= ".." && d /= ".mdb")) $ catMaybes subs
