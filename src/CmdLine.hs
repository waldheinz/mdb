
module CmdLine (
  Mode(..), OptInit(..), parseMode
  ) where

data Mode
     = ModeInit OptInit
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("init" : args) = case parseModeInit args of
  Right oi -> Right $ ModeInit oi
  Left e   -> Left e

parseMode (x:_) = Left $ "unknown mode " ++ x
parseMode [] = Left "no mode given"

data OptInit = OptInit
               { initDir :: Maybe FilePath }
               deriving ( Show )

parseModeInit :: [String] -> Either String OptInit
parseModeInit args = case args of
  []     -> Right $ OptInit Nothing
  (x:[]) -> Right $ OptInit $ Just x
  xx     -> Left  $ "don't know how to init " ++ (show xx)
  
