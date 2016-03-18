
{-# LANGUAGE OverloadedStrings #-}

module Mdb.TvShow ( doMode ) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Either  (left, runEitherT)
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Network.HTTP.Client.Conduit (Manager, httpLbs, parseUrl,
                                              responseBody, withManager)
import           System.FilePath             (splitDirectories)
import qualified Text.XML.Light              as XML
import qualified Text.XML.Light.Proc         as XML

import           Mdb.CmdLine                 (OptTvShow (..))
import           Mdb.Database

tvDbApiKey :: String
tvDbApiKey = "2FA137A801CEDA74"

tvDbApiBase :: String
tvDbApiBase = "http://thetvdb.com/api/"

doMode :: OptTvShow -> MDB IO ()
doMode (AssignTvShow lang folders) = withManager $ mapM_ (scanShow lang) folders

eName :: String -> XML.QName
eName n = XML.QName n Nothing Nothing

scanShow :: String -> FilePath -> ReaderT Manager (MDB IO) ()
scanShow lang showDir = do
    let
        dirs = splitDirectories showDir
        dirName = last dirs

    liftIO $ putStrLn $ "fetching info for \"" ++ last dirs ++ "\""

    resp <- parseUrl (tvDbApiBase ++ "GetSeries.php?seriesname=" ++ dirName ++ "&language=" ++ lang) >>= httpLbs

    case (XML.parseXMLDoc . decodeUtf8 . BSL.toStrict . responseBody) resp of
        Nothing     -> liftIO $ putStrLn "could not parse XML response"
        Just xml    -> do
            let
                found = XML.findChildren (eName "Series") xml

            case length found of
                0   -> liftIO $ putStrLn "no candidate found"
                1   -> assignShowFromXml showDir $ head found
                _   -> liftIO $ putStrLn "multiple matches"

assignShowFromXml :: FilePath -> XML.Element -> ReaderT Manager (MDB IO) ()
assignShowFromXml showDir xml = do

    let
        mdata = do
            name    <- XML.strContent <$> XML.findChild (eName "SeriesName") xml
            desc    <- XML.strContent <$> XML.findChild (eName "Overview") xml
            tvdb_id <- XML.strContent <$> XML.findChild (eName "seriesid") xml
            return (name, desc, tvdb_id)

    case mdata of
        Nothing -> liftIO $ putStrLn "could not extract data"
        Just (name, desc, tvdb_id) -> do
            showId <- lift $ do
                relDir <- relFile showDir
                dbExecute ("INSERT OR REPLACE INTO tv_show "
                    <> "(tv_show_id, tv_show_name, tv_show_description, tv_show_tvdb_id, tv_show_root) "
                    <> "VALUES ((SELECT tv_show_id FROM tv_show WHERE tv_show_root = ?), ?, ?, ?, ?)")
                    (showDir, name, desc, tvdb_id, relDir)
                dbLastRowId

            liftIO $ putStrLn $ "inserted as \"" ++ name ++ "\" with ID " ++ show showId
