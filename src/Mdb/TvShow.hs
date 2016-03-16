
module Mdb.TvShow ( doMode ) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Either  (left, runEitherT)
import           Control.Monad.Trans.Reader  (ReaderT)
import qualified Data.ByteString.Lazy as BSL
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
doMode opts = withManager $ mapM_ (scanShow $ tvShowLanguage opts) $ tvShowFolders opts

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
            liftIO $ putStrLn $ XML.showElement xml
