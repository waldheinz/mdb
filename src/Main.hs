
module Main (
  main
  ) where

import qualified CmdLine  as CMD
import qualified Database as DB

import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

main :: IO ()
main = do
  argv <- getArgs
  
  case CMD.parseMode argv of
    Left x     -> putStrLn $ x
    Right mode -> case mode of
      CMD.ModeInit oi -> doInit oi
--      x               -> error $ "unimplemented mode " ++ (show x)
    
  {-
  mdbf <- DB.findDbFolder
  case mdbf of
    Nothing  -> putStrLn $ "no db directory found"
    Just dbf -> do 
      putStrLn $ show dbf
      db <- DB.openDb dbf
      return ()
  -}

doInit :: CMD.OptInit -> IO ()
doInit (CMD.OptInit mp) = do
  p <- case mp of
    Just ap -> return ap
    Nothing -> getCurrentDirectory

  DB.initDb p
  
