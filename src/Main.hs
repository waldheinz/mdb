
module Main (
  main
  ) where

import qualified CmdLine  as CMD
--import qualified Database as DB

import System.Environment ( getArgs )

main :: IO ()
main = do
  argv <- getArgs
  
  case CMD.parseMode argv of
    Left x -> putStrLn $ x
    Right m -> error $ show m
    
  {-
  mdbf <- DB.findDbFolder
  case mdbf of
    Nothing  -> putStrLn $ "no db directory found"
    Just dbf -> do 
      putStrLn $ show dbf
      db <- DB.openDb dbf
      return ()
  -}
