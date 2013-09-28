
module CmdLine (
  Mode(..), parseMode
  ) where

data Mode
     = Init { initDir :: Maybe FilePath }
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("init" : args) = parseModeInit args
parseMode (x:_) = Left $ "unknown mode " ++ x
parseMode [] = Left "no mode given"

parseModeInit :: [String] -> Either String Mode
parseModeInit argv = Right $ Init Nothing
