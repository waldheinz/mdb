
module CmdLine (
  Mode(..), OptInit(..), OptScan(..), OptPerson(..), parseMode
  ) where

import Mdb.Database.Person ( PersonId )

data Mode
     = ModeInit OptInit
     | ModeScan OptScan
     | ModePerson OptPerson
     | ModeServe
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("person" : args) = parseModePerson args >>= return . ModePerson
parseMode ("init" : args) = case parseModeInit args of
  Right oi -> Right $ ModeInit oi
  Left e   -> Left e
parseMode ("scan" : args) = case parseModeScan args of
  Right os -> Right $ ModeScan os
  Left e   -> Left e
parseMode ("serve" : _) = Right ModeServe
parseMode (x:_) = Left $ "unknown mode " ++ x
parseMode [] = Left "no mode given"

-----------------------------------------------------
-- init
-----------------------------------------------------

data OptInit = OptInit
               { initDir :: Maybe FilePath }
               deriving ( Show )

parseModeInit :: [String] -> Either String OptInit
parseModeInit args = case args of
  []     -> Right $ OptInit Nothing
  (x:[]) -> Right $ OptInit $ Just x
  xx     -> Left  $ "don't know how to init " ++ (show xx)
  
----------------------------------------------------
-- scan
----------------------------------------------------

data OptScan = OptScan
               deriving ( Show )

parseModeScan :: [String] -> Either String OptScan
parseModeScan _ = Right OptScan

----------------------------------
-- person
----------------------------------

data OptPerson
    = AddPerson String
    | SetPersonImage PersonId FilePath
    deriving ( Show )

parseModePerson :: [String] -> Either String OptPerson
parseModePerson ("add" : name : _) = Right $ AddPerson name
parseModePerson (pids : "image" : fname: _) = Right $ SetPersonImage (read pids) fname
parseModePerson _ = Left "you want to do something with a person, but I fail to understand what"