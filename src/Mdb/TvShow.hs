
{-# LANGUAGE OverloadedStrings #-}

module Mdb.TvShow ( doMode ) where

import           Control.Monad               ( forM_ )
import           Control.Monad.Catch         (MonadCatch, try)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Reader.Class  (asks)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable (traverse_)
import Data.Int ( Int64 )
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Network.HTTP.Conduit        ( HttpException )
import           Network.HTTP.Client.Conduit (Manager, Response, httpLbs, parseUrl,
                                              responseBody, withManager)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (splitDirectories, takeExtension, takeDirectory)
import qualified Text.XML.Light              as XML
import qualified Text.XML.Light.Proc         as XML

import           Mdb.CmdLine                 (OptTvShow (..))
import           Mdb.Database
import           Mdb.Database.File           (FileId)
import           Mdb.File                    (checkFile)

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
        Right ()    -> liftIO $ putStrLn "ok"

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

    liftIO $ putStrLn $ "fetching info for \"" ++ last dirs ++ "\""

    resp <- parseUrl (tvDbApiBase ++ "GetSeries.php?seriesname=" ++ dirName ++ "&language=" ++ lang) >>= httpLbs
    xml <- xmlBody resp

    let
        found = XML.findChildren (eName "Series") xml

    case found of
        []  -> left "no candidate found"
        [x] -> EitherT $ assignShowFromXml lang showDir x
        _   -> left "multiple matches"

assignShowFromXml :: String -> FilePath -> XML.Element -> ReaderT Manager (MDB IO) (Either T.Text ())
assignShowFromXml lang showDir xml = runEitherT $ do

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
            -- traverse_ (updatePoster showId) $ XML.strContent <$> XML.findChild (eName "banner") xml

childElem :: Monad m => T.Text -> XML.Element -> EitherT T.Text m XML.Element
childElem name xml = case XML.findChild (eName $ T.unpack name) xml of
    Nothing -> left $ "could not find \"" <> name <> "\" child in XML"
    Just x -> right x

textChild :: Monad m => T.Text -> XML.Element -> EitherT T.Text m String
textChild name xml = XML.strContent <$> childElem name xml

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


    mposterId <- lift $ updateImage ("posters/" ++ show showId) seriesPoster
    case mposterId of
        Left _      -> liftIO $ putStrLn "fetching poster image failed"
        Right fid   -> lift . lift $ dbExecute "UPDATE series SET series_poster = ? WHERE series_id = ?" (fid, showId)


    forM_ (XML.findChildren (eName "Episode") fullXml) $ \xml -> do

        --liftIO $ putStrLn (XML.showTopElement xml)
        return ()

updateImage :: (MonadCatch m, MonadIO m) => FilePath -> String -> ReaderT Manager (MDB m) (Either T.Text FileId)
updateImage destFile banner = runEitherT $ do
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
