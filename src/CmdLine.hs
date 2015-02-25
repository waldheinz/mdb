
module CmdLine (
    Mode(..), OptAlbum(..), OptInit(..), OptPerson(..), OptFile(..), parseCommandLine,
    cmdLineParser
    ) where

import Options.Applicative

import Mdb.Database.Album ( AlbumId )
import Mdb.Database.Person ( PersonId )

data Mode
    = ModeInit OptInit
    | ModeAlbum OptAlbum
    | ModeFile OptFile Bool [FilePath]
    | ModePerson OptPerson
    | ModeServe
    deriving ( Show )

data OptInit = OptInit
    { initDir :: Maybe FilePath }
    deriving ( Show )

data OptAlbum
    = AlbumCreate String
    deriving ( Show )

albumOptions :: Parser Mode
albumOptions = ModeAlbum
    <$> subparser
        (   command "create"    ( info
            (AlbumCreate <$> strArgument ( metavar "NAME" ) )
            (progDesc "create album") )
        )

data OptFile
    = FileAdd
    | FileAssignPerson PersonId
    | FileAssignAlbum  AlbumId
    deriving ( Show )

fileOptions :: Parser Mode
fileOptions = ModeFile
    <$> subparser
        (   command "add"       (info
            (pure FileAdd)
            (progDesc "add files") )
        <>  command "assign"    (info
            (helper <*> fileAssign)
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

fileAssign :: Parser OptFile
fileAssign = subparser
    (   command "person"    (info
        (FileAssignPerson <$> argument auto ( metavar "PID") )
        (progDesc "assign files to a person") )
    <>  command "album"     (info
        (FileAssignAlbum <$> argument auto ( metavar "AID") )
        (progDesc "assign files to an album") )
    )

data OptPerson
    = AddPerson String
    | SetPersonImage PersonId FilePath
    deriving ( Show )

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
    <>  command "album" (info (helper <*> albumOptions)
            ( progDesc "Manage albums" ) )
    )

parseCommandLine :: IO Mode
parseCommandLine = execParser opts
  where
    opts = info (helper <*> cmdLineParser)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
