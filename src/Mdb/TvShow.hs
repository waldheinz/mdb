
{-# LANGUAGE OverloadedStrings #-}

module Mdb.TvShow ( doMode ) where

import           Control.Monad.Catch         (MonadCatch, try)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader.Class  (asks)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Char ( toLower )
import           Data.Int                    (Int64)
import           Data.List                   (nub, sort)
import           Data.Maybe                  (mapMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Network.HTTP.Client.Conduit (Manager, Response, httpLbs,
                                              parseUrl, responseBody,
                                              withManager)
import           Network.HTTP.Conduit        (HttpException)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (splitDirectories, takeDirectory,
                                              takeExtension)
import qualified Text.XML.Light              as XML

import           Mdb.CmdLine                 (OptTvShow (..))
import           Mdb.Database
import           Mdb.File                    (checkFile)
import           Mdb.Types

tvDbApiKey :: String
tvDbApiKey = "2FA137A801CEDA74"

tvDbBase :: String
tvDbBase = "http://thetvdb.com/"

tvDbApiBase :: String
tvDbApiBase = tvDbBase ++ "api/"

authBase :: String
authBase = tvDbApiBase  ++ tvDbApiKey ++ "/"

doMode :: OptTvShow -> MDB IO ()
doMode (AssignTvShow lang folders) = withManager $ mapM_ go folders where
    go folder = scanShow lang folder >>= \x -> case x of
        Left msg    -> liftIO $ putStrLn $ T.unpack $ "error: " <> msg
        Right ()    -> return ()

eName :: String -> XML.QName
eName n = XML.QName n Nothing Nothing

xmlBody :: Monad m => Response BSL.ByteString -> EitherT T.Text m XML.Element
xmlBody resp = case (XML.parseXMLDoc . decodeUtf8 . BSL.toStrict . responseBody) resp of
    Nothing     -> left "failed to parse response as XML"
    Just xml    -> right xml

scanShow :: String -> FilePath -> ReaderT Manager (MDB IO) (Either T.Text ())
scanShow lang showDir = runEitherT $ do
    let
        dirs = splitDirectories showDir
        dirName = last dirs

    esid <- lift . lift $ do
        rel <- relFile showDir
        dbQueryOne "SELECT series_id, series_name FROM series WHERE ? LIKE (series_root || '%')" (Only rel)

    case esid of
        Right (serId, name) -> liftIO $ putStrLn $ showDir ++ ": already assigned to \""
                                                ++ name ++ "\" (" ++ show (serId :: SerialId) ++ ")"

        Left _              -> do
            liftIO $ putStrLn $ "fetching info for \"" ++ last dirs ++ "\""

            xml <- parseUrl (tvDbApiBase ++ "GetSeries.php?seriesname=" ++ dirName ++ "&language=" ++ lang)
                >>= httpLbs >>= xmlBody

            case XML.findChildren (eName "Series") xml of
                []  -> left $ "no candidate found for \"" <> T.pack dirName <> "\""
                [x] -> assign lang showDir x
                xs  -> pick xs dirName lang >>= assign lang showDir

pick :: [XML.Element] -> String -> String -> EitherT T.Text (ReaderT Manager (MDB IO)) XML.Element
pick els dirName lang = do
    let
        lCase           = map toLower
        field name xml  = XML.strContent <$> XML.findChild (eName name) xml
        wrap xml        =
            (,)
                <$> ((,,) <$> field "SeriesName" xml <*> field "FirstAired" xml <*> field "language" xml)
                <*> pure xml
        wrapped         = mapMaybe wrap els
        langMatch       = filter (\((_, _, l), _) -> lCase lang == lCase l) wrapped
        exactName       = filter (\((n, _, _), _) -> lCase dirName == lCase n) langMatch

    case exactName of
        []  -> left $ "no plausible candidates left, started with: " <> T.pack (show $ map fst wrapped)
        [x] -> return $ snd x
        _   -> left $ "still multiple candidates left: " <> T.pack (show $ map fst exactName)

assign :: (MonadCatch m, MonadIO m) => String -> FilePath -> XML.Element -> EitherT T.Text (ReaderT Manager (MDB m)) ()
assign lang showDir xml = do

    let
        mdata = do
            name    <- XML.strContent <$> XML.findChild (eName "SeriesName") xml
            desc    <- XML.strContent <$> XML.findChild (eName "Overview") xml
            tvdb_id <- XML.strContent <$> XML.findChild (eName "seriesid") xml
            return (name, desc, tvdb_id)

    case mdata of
        Nothing -> left "could not extract data"
        Just (name, desc, tvdb_id) -> do
            showId <- lift . lift $ do
                relDir <- relFile showDir
                dbExecute ("INSERT OR REPLACE INTO series "
                    <> "(series_id, series_name, series_description, series_tvdb_id, series_root, series_lang) "
                    <> "VALUES ((SELECT series_id FROM series WHERE series_root = ?), ?, ?, ?, ?, ?)")
                    (relDir, name, desc, tvdb_id, relDir, lang)
                dbLastRowId

            liftIO $ putStrLn $ "inserted as \"" ++ name ++ "\" with ID " ++ show showId
            updateSeries showId

childElem :: Monad m => T.Text -> XML.Element -> EitherT T.Text m XML.Element
childElem name xml = case XML.findChild (eName $ T.unpack name) xml of
    Nothing -> left $ "could not find \"" <> name <> "\" child in XML"
    Just x -> right x

textChild :: Monad m => T.Text -> XML.Element -> EitherT T.Text m String
textChild name xml = XML.strContent <$> childElem name xml

readChild :: (Monad m, Read a) => T.Text -> XML.Element -> EitherT T.Text m a
readChild name xml = textChild name xml >>= \str -> case reads str of
    []          -> left $ "could not parse \"" <> T.pack str <> "\" (" <> name <> ")"
    [(a, _)]    -> right a
    _           -> left $ "there are multiple parses for \"" <> T.pack str <> "\" (" <> name <> ")"

updateSeries :: (MonadIO m, MonadCatch m) => Int64 ->  EitherT T.Text (ReaderT Manager (MDB m)) ()
updateSeries showId = do
    (tvDbId, lang) <- EitherT $ lift $ dbQueryOne
        "SELECT series_tvdb_id, series_lang FROM series WHERE series_id = ?" (Only showId)

    fullXml <- parseUrl (authBase ++ "series/" ++ show (tvDbId :: Int64) ++ "/all/" ++ (lang :: String) ++ ".xml")
                >>= httpLbs >>= xmlBody

    series <- childElem "Series" fullXml
    seriesName <- textChild "SeriesName" series
    seriesDesc <- textChild "Overview" series
    seriesPoster <- textChild "poster" series

    lift . lift $ dbExecute
        "UPDATE series SET series_name = ?, series_description = ? WHERE series_id = ?"
        (seriesName, seriesDesc, showId)


    mposterId <- lift $ runEitherT $ updateImage ("posters/" ++ show showId) seriesPoster
    case mposterId of
        Left _      -> liftIO $ putStrLn "fetching poster image failed"
        Right fid   -> lift . lift $ dbExecute "UPDATE series SET series_poster = ? WHERE series_id = ?" (fid, showId)

    let
        oneEpisode exml = do
            seasonNum   <- readChild "SeasonNumber" exml
            episodeNum  <- readChild "EpisodeNumber" exml
            desc        <- textChild "Overview" exml
            title       <- textChild "EpisodeName" exml

            lift . lift $ do
                dbExecute "INSERT OR IGNORE INTO series_season (series_id, series_season_number) VALUES (?, ?)"
                    (showId, seasonNum :: Int64)

                dbExecute
                    (   "INSERT OR REPLACE INTO series_episode "
                    <>  "(series_id, series_season_number, series_episode_number, "
                    <>      "series_episode_title, series_episode_description) "
                    <>  "VALUES (?, ?, ?, ?, ?)"
                    ) (showId, seasonNum, episodeNum :: Int64, title, desc)

            return seasonNum

        onePoster banners seasonId = do
            let
                flt b = case b of
                    SeasonPoster _ sid _    -> seasonId == sid

            case sort $ filter flt banners of
                []  -> return ()
                xs  -> do
                    let
                        (SeasonPoster _ _ path) = last xs

                    pid <- lift $ runEitherT $ updateImage ("posters/" ++ show showId ++ "-" ++ show seasonId) path
                    case pid of
                        Left _      -> liftIO $ putStrLn "fetching poster image failed"
                        Right fid   -> lift . lift $ dbExecute
                            (   "UPDATE series_season SET series_season_poster = ? "
                            <>  "WHERE series_id = ? AND series_season_number = ?") (fid, showId, seasonId)

    seasonIds <- mapM oneEpisode $ XML.findChildren (eName "Episode") fullXml
    bannersXml <- parseUrl (authBase ++ "series/" ++ show tvDbId ++ "/banners.xml")
                >>= httpLbs >>= xmlBody

    mapM_ (onePoster $ parseBanners bannersXml) (nub seasonIds)

updateImage :: (MonadCatch m, MonadIO m) => FilePath -> String -> EitherT T.Text (ReaderT Manager (MDB m)) FileId
updateImage destFile banner = do
    dbDir <- lift . lift $ asks mdbDbDir

    let
        fileExt = takeExtension banner
        dstFile = dbDir ++ "/series/" ++ destFile ++ fileExt
        dstPath = takeDirectory dstFile
        pUrl    = tvDbBase ++ "banners/"++ banner

    liftIO $ createDirectoryIfMissing True dstPath
    liftIO $ putStrLn $ "fetching image " ++ pUrl

    success <- try $ do
        resp <- parseUrl pUrl >>= httpLbs
        liftIO $ BSL.writeFile dstFile (responseBody resp)

    case success of
        Right ()    -> return ()
        Left he     -> left (T.pack $ show (he :: HttpException))

    EitherT $ lift $ checkFile dstFile

------------------------------------------------------------------------------------------------------------------------
-- dealing with banners.xml
------------------------------------------------------------------------------------------------------------------------

data Banner
    = SeasonPoster !Double !Int64 !String   -- ^ rating , season_id, path
    deriving ( Eq, Show )

instance Ord Banner where
    compare b1 b2 = compare (bannerRating b1) (bannerRating b2)

bannerRating :: Banner -> Double
bannerRating (SeasonPoster r _ _) = r

extractBanner :: XML.Element -> Maybe Banner
extractBanner xml =
    let
        tp      = XML.strContent <$> XML.findChild (eName "BannerType") xml
        tp2     = XML.strContent <$> XML.findChild (eName "BannerType2") xml
        path    = XML.strContent <$> XML.findChild (eName "BannerPath") xml
        rating  = XML.findChild (eName "Rating") xml >>= pread
        season  = XML.findChild (eName "Season") xml >>= pread
        pread x = case reads (XML.strContent x) of
            []          -> Nothing
            [(r, _)]    -> Just r
            _           -> Nothing
    in
        do
            "season"    <- tp
            "season"    <- tp2
            SeasonPoster <$> rating <*> season <*> path

parseBanners :: XML.Element -> [Banner]
parseBanners xml = mapMaybe extractBanner $ XML.findChildren (eName "Banner") xml
