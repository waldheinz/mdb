
module Mdb.CmdLine (
    MdbOptions(..),
    Mode(..), OptAlbum(..), OptPerson(..), OptUser(..), OptTvShow(..),
    OptFile(..), AssignTarget(..), parseCommandLine
    ) where

import           Options.Applicative

import           Mdb.Types

data Mode
    = ModeInit
    | ModeAlbum OptAlbum
    | ModeFile OptFile Bool [FilePath]
    | ModePerson OptPerson
    | ModeServe
    | ModeTvShow OptTvShow
    | ModeUser OptUser
    deriving ( Show )

initOptions :: Parser Mode
initOptions = pure ModeInit

data OptTvShow
    = AssignTvShow
        { tvShowLanguage :: String
        , tvShowFolders  :: [FilePath]
        } deriving ( Show )

tvShowOptions :: Parser Mode
tvShowOptions = ModeTvShow
    <$> subparser
        ( command "assign"
            ( info
                ( AssignTvShow
                    <$> strArgument ( metavar "LANG" <> help "Language to use" )
                    <*> some (strArgument ( metavar "FOLDERS..." <> help "Specify TV show folders" ))
                ) ( progDesc "assign TV shows to folders" )
            )
        )

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

data AssignTarget
    = AssignPerson PersonId
    | AssignNewPerson String
    | AssignAlbum AlbumId
    | AssignNewAlbum String
    deriving ( Show )

data OptFile
    = FileAdd
    | FileAssign [AssignTarget]
    | FileScan { scanSha1 :: Bool }
    deriving ( Show )

fileOptions :: Parser Mode
fileOptions = ModeFile
    <$> subparser
        (   command "add"       (info
            (helper <*> pure FileAdd)
            (progDesc "add files") )
        <>  command "assign"    (info
            (helper <*> fileAssign)
            (progDesc "assign files to persons, albums, ...")
            )
        <>  command "scan" ( info
            (helper <*> fileScan)
            (progDesc "scan files for hashes, resolution, ...")
            )
        )
    <*> switch
        (   long "recursive"
        <>  short 'r'
        <>  help "apply to files in subdirectories"
        )
    <*> (some . strArgument)
        ( metavar "FILES..." )

fileScan :: Parser OptFile
fileScan = FileScan
        <$> switch
            (   long "sha1"
            <>  help "calculate SHA1 for files"
            )

fileAssign :: Parser OptFile
fileAssign = FileAssign <$> some (p <|> np <|> a <|> na) where
    p = AssignPerson <$> option auto
        (   long "person"
        <>  short 'p'
        <>  help "assign to existing person by id"
        <>  metavar "PID"
        )
    np = AssignNewPerson <$> strOption
        (   long "new-person"
        <>  help "assign new person"
        <>  metavar "NAME"
        )
    a = AssignAlbum <$> option auto
        (   long "album"
        <>  short 'a'
        <>  help "assign to existing album by id"
        <>  metavar "AID"
        )
    na = AssignNewAlbum <$> strOption
        (   long "new-album"
        <>  help "assign new album"
        <>  metavar "NAME"
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

data OptUser
    = AddUser String
    deriving ( Show )

userOptions :: Parser Mode
userOptions = ModeUser
    <$> subparser
        (   command "add"   ( info
                (AddUser <$> strArgument ( metavar "<USERNAME>" ))
                ( progDesc "add a new user for the web UI, will prompt for the password" )
            )
        )

modeParser :: Parser Mode
modeParser = subparser
    (   command "file" (info (helper <*> fileOptions)
            ( progDesc "Manage files in the database" ))
    <>  command "init" (info (helper <*> initOptions)
            ( progDesc "Initialize database" ))
    <>  command "person" (info (helper <*> personOptions)
            ( progDesc "Manage persons in the database" ))
    <>  command "serial" (info (helper <*> tvShowOptions)
            ( progDesc "Manage TV serials"))
    <>  command "serve" (info (helper <*> serveOptions)
            ( progDesc "Start HTTP server" ))
    <>  command "album" (info (helper <*> albumOptions)
            ( progDesc "Manage albums" ) )
    <>  command "user" (info (helper <*> userOptions)
            ( progDesc "Manage Users" ))
    )

data MdbOptions = MdbOptions
    { rootDir :: Maybe FilePath -- ^ root of the MDB tree (the directory containing the ".mdb" directory)
    , mode    :: Mode
    } deriving ( Show )

parseCommandLine :: IO MdbOptions
parseCommandLine = execParser opts
    where
        opts = info (helper <*> cmdLineParser)
            ( fullDesc
            <>  progDesc "Print a greeting for TARGET"
            <>  header "hello - a test for optparse-applicative"
            )

        cmdLineParser = MdbOptions
            <$> optional (strOption ( long "root" <> metavar "DIR" <> help "Specify base directory" ))
            <*> modeParser
