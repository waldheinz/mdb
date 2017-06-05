
module Mdb.CmdLine (
    MdbOptions(..),
    Mode(..), OptAlbum(..), OptPerson(..), OptUser(..), OptStatus(..), OptTvShow(..),
    OptFile(..), ScanFlags(..), AssignTarget(..), parseCommandLine
    ) where

import qualified Control.Monad.Logger as LOG
import           Data.List ( find, intercalate )
import           Data.Monoid ( (<>) )
import           Options.Applicative

import           Mdb.Types

------------------------------------------------------------------------------------------------------------------------
-- Top-Level Cmdline
------------------------------------------------------------------------------------------------------------------------

data Mode
    = ModeAlbum OptAlbum
    | ModeFile OptFile Bool [FilePath]
    | ModeInit
    | ModePerson OptPerson
    | ModeServe
    | ModeStatus OptStatus
    | ModeTvShow OptTvShow
    | ModeUser OptUser
    deriving ( Show )

initOptions :: Parser Mode
initOptions = pure ModeInit


modeParser :: Parser Mode
modeParser = subparser
    (   command "album" (info (helper <*> albumOptions)
            ( progDesc "Manage albums" ) )
    <>  command "file" (info (helper <*> fileOptions)
            ( progDesc "Manage files in the database" ))
    <>  command "init" (info (helper <*> initOptions)
            ( progDesc "Initialize database" ))
    <>  command "person" (info (helper <*> personOptions)
            ( progDesc "Manage persons in the database" ))
    <>  command "serial" (info (helper <*> tvShowOptions)
            ( progDesc "Manage TV serials"))
    <>  command "serve" (info (helper <*> serveOptions)
            ( progDesc "Start HTTP server" ))
    <>  command "status" (info (helper <*> statusOptions)
            ( progDesc "Verify integrity of database"))
    <>  command "user" (info (helper <*> userOptions)
            ( progDesc "Manage Users" ))
    )

data MdbOptions = MdbOptions
    { logLevel  :: LOG.LogLevel
    , rootDir   :: Maybe FilePath -- ^ root of the MDB tree (the directory containing the ".mdb" directory)
    , mode      :: Mode
    } deriving ( Show )

levelMap :: [ ( String, LOG.LogLevel ) ]
levelMap =
    [ ( "debug" , LOG.LevelDebug )
    , ( "info"  , LOG.LevelInfo )
    , ( "warn"  , LOG.LevelWarn )
    , ( "error" , LOG.LevelError )
    ]

parseLevel :: ReadM LOG.LogLevel
parseLevel = eitherReader go where
    go s = maybe (Left $ "valid levels: " ++ valid) Right $ fmap snd (find (\(ss, _) -> s == ss) levelMap)
    valid = intercalate ", " $ map fst levelMap

parseCommandLine :: IO MdbOptions
parseCommandLine = execParser opts
    where
        opts = info (helper <*> cmdLineParser)
            ( fullDesc
            <>  progDesc "Manage your media files"
            <>  header "mdb - Media DataBase"
            )

        cmdLineParser = MdbOptions
            <$> option parseLevel
                (   value LOG.LevelInfo
                <>  showDefaultWith (\l -> maybe "<don't know>" fst $ find (\(_, x) -> x == l) levelMap)
                <>  long "verbose"
                <>  short 'v'
                <>  metavar "LEVEL"
                <>  help "Specify log level"
                )
            <*> optional (strOption ( long "root" <> metavar "DIR" <> help "Specify base directory" ))
            <*> modeParser

------------------------------------------------------------------------------------------------------------------------
-- Album
------------------------------------------------------------------------------------------------------------------------

data OptAlbum
    = AlbumCreate String
    | AlbumRemoveEmpty
    deriving ( Show )

albumOptions :: Parser Mode
albumOptions = ModeAlbum <$> (create <|> removeEmpty) where
    create = AlbumCreate <$> strOption ( long "create" <> metavar "NAME" <> help "create empty album" )
    removeEmpty = flag' AlbumRemoveEmpty ( long "remove-empty" <> help "remove all empty albums" )

------------------------------------------------------------------------------------------------------------------------
-- File
------------------------------------------------------------------------------------------------------------------------

data AssignTarget
    = AssignPerson PersonId
    | AssignNewPerson String
    | AssignAlbum AlbumId
    | AssignNewAlbum String
    | AssignTag String
    deriving ( Show )

data ScanFlags = ScanFlags
    { scanSha1      :: Bool
    , scanThumbs    :: Bool
    } deriving ( Show )

data OptFile
    = FileAdd ScanFlags
    | FileAssign [AssignTarget]
    | FileScan ScanFlags
    deriving ( Show )

fileOptions :: Parser Mode
fileOptions = ModeFile <$> cmd <*> rec <*> files where
    cmd = subparser
        (   command "add"       (info
            (helper <*> (FileAdd <$> fileScanFlags ) )
            (progDesc "add and scan new files") )
        <>  command "assign"    (info
            (helper <*> fileAssign )
            (progDesc "assign files to persons, albums, ...")
            )
        <>  command "scan" ( info
            (helper <*> (FileScan <$> fileScanFlags) )
            (progDesc "scan files for hashes, resolution, ...")
            )
        )
    rec = switch ( long "recursive" <>  short 'r' <>  help "apply to files in subdirectories" )
    files = (some . strArgument) ( metavar "FILES..." )

fileScanFlags :: Parser ScanFlags
fileScanFlags = ScanFlags
        <$> switch ( long "sha1" <> help "calculate SHA1 for files" )
        <*> switch ( long "thumbs" <> help "generate thumbnails" )

fileAssign :: Parser OptFile
fileAssign = FileAssign <$> some (p <|> np <|> a <|> na <|> tag) where
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
    tag = AssignTag <$> strOption
        (   long "tag"
        <>  short 't'
        <>  help "assign a tag"
        <>  metavar "NAME"
        )

------------------------------------------------------------------------------------------------------------------------
-- Person
------------------------------------------------------------------------------------------------------------------------

data OptPerson
    = AddPerson String
    | PersonList
    | SetPersonPortrait PersonId FileId
    deriving ( Show )


personOptions :: Parser Mode
personOptions = ModePerson <$> (add <|> list <|> portrait) where
    add       = AddPerson <$> strOption ( long "add" <> metavar "NAME" <> help "add new person" )
    list      = flag' PersonList (long "list" <> help "list persons")
    portrait  = SetPersonPortrait <$> option auto ( metavar "PID" ) <*> argument auto ( metavar "FID" )

{-
personOptions :: Parser Mode
personOptions = ModePerson
    <$> subparser
        (   command "add"   ( info
                ( AddPerson <$> strArgument ( metavar "NAME" ) )
                ( progDesc "add person" )
            )
        <>  command "portrait" ( info
                ( SetPersonPortrait <$> argument auto ( metavar "PID" ) <*> argument auto ( metavar "FID" ) )
                ( progDesc "set a person's portrait image by person and file IDs" )
            )
        )
-}

------------------------------------------------------------------------------------------------------------------------
-- Serve
------------------------------------------------------------------------------------------------------------------------

serveOptions :: Parser Mode
serveOptions = pure ModeServe

------------------------------------------------------------------------------------------------------------------------
-- Status
------------------------------------------------------------------------------------------------------------------------

newtype OptStatus = OptStatus
    { removeMissing :: Bool
    } deriving ( Show )

optStatusParser :: Parser OptStatus
optStatusParser = OptStatus <$> switch ( long "remove" <> help "remove missing files from DB" )

statusOptions :: Parser Mode
statusOptions = ModeStatus <$> optStatusParser

------------------------------------------------------------------------------------------------------------------------
-- Tv Show
------------------------------------------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------------------------------
-- User
------------------------------------------------------------------------------------------------------------------------

newtype OptUser
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
