
module CmdLine (
    Mode(..), OptInit(..), OptPerson(..), OptFile(..), parseCommandLine,
    cmdLineParser
    ) where

import Mdb.Database.Person ( PersonId )

import Options.Applicative

data Mode
     = ModeInit OptInit
     | ModeFile OptFile Bool [FilePath]
     | ModePerson OptPerson
     | ModeServe
     deriving ( Show )

data OptFile
    = FileAdd
    | FileAssignPerson PersonId
    deriving ( Show )

data OptInit = OptInit
               { initDir :: Maybe FilePath }
               deriving ( Show )

data OptPerson
    = AddPerson String
    | SetPersonImage PersonId FilePath
    deriving ( Show )

fileOptions :: Parser Mode
fileOptions = ModeFile
    <$> subparser
        (   command "add"       (info
            (pure FileAdd)
            (progDesc "add files") )
        <>  command "assign"    (info
            (FileAssignPerson <$> argument auto ( metavar "PID") )
            (progDesc "assign person")
            )
        )
    <*> switch
        (   long "recursive"
        <>  short 'r'
        <>  help "apply to files in subdirectories"
        )
    <*> (some . strArgument)
        ( metavar "FILES..." )

personOptions :: Parser Mode
personOptions = ModePerson
    <$> subparser
        (   command "add"   ( info
                ( AddPerson <$> strArgument ( metavar "NAME" ) )
                ( progDesc "add person" )
            )

        <>  command "setimage"   ( info
                ( SetPersonImage
                    <$> argument auto ( metavar "PID" )
                    <*> strArgument ( metavar "FILE" )
                )
                ( progDesc "set image" )
            )
        )

serveOptions :: Parser Mode
serveOptions = pure ModeServe

cmdLineParser :: Parser Mode
cmdLineParser = subparser
    (   command "file" (info (helper <*> fileOptions)
            ( progDesc "Manage files in the database" ))
    <>  command "person" (info (helper <*> personOptions)
            ( progDesc "Manage persons in the database" ))
    <>  command "serve" (info (helper <*> serveOptions)
            ( progDesc "Start HTTP server" ))
    )

parseCommandLine :: IO Mode
parseCommandLine = execParser opts
  where
    opts = info (helper <*> cmdLineParser)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
