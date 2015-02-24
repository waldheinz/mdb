
module CmdLine (
  Mode(..), OptInit(..), OptPerson(..), OptFile(..), parseMode
  ) where

import Mdb.Database.Person ( PersonId )

data Mode
     = ModeInit OptInit
     | ModeFile OptFile
     | ModePerson OptPerson
     | ModeServe
     deriving ( Show )

parseMode :: [String] -> Either String Mode
parseMode ("file"   : args  ) = parseModeFile args >>= return . ModeFile
parseMode ("person" : args  ) = parseModePerson args >>= return . ModePerson
parseMode ("init"   : args  ) = parseModeInit args >>= return . ModeInit
parseMode ("serve"  : _     ) = Right ModeServe
parseMode (x        : _     ) = Left $ "unknown mode " ++ x
parseMode []                  = Left "no mode given"

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
-- file
----------------------------------------------------

data OptFile
    = FileAdd [FilePath]
    | FileAssignPerson PersonId [FilePath]
    deriving ( Show )

parseModeFile :: [String] -> Either String OptFile
parseModeFile ( "add"       : fs ) = Right $ FileAdd fs
parseModeFile ( "assign"    : "person" : pids : fs ) = Right $ FileAssignPerson (read pids) fs
parseModeFile _ = Left "don't know what you want to do with files"

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
