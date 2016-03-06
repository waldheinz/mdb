
{-# LANGUAGE OverloadedStrings #-}

module Serve (
    doServe
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad ( (>=>) )
import Control.Monad.Catch ( MonadMask )
import Control.Monad.Reader.Class ( ask )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy as BSL
import Data.String ( fromString )
import           Data.Text.Encoding ( encodeUtf8 )
import Network.HTTP.Types ( status200, status404 )
import Network.Wai ( Application, responseLBS, responseBuilder )
import Network.Wai.Application.Static ( defaultFileServerSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import Network.Wai.UrlMap ( mapUrls, mount, mountRoot )
import           Heist as HEIST
import           Heist.Interpreted as HEIST

import Mdb.Serve.Image
import Mdb.Serve.Video
import Mdb.Templates
import Database
import Paths_mdb ( getDataDir )
import RestApi ( apiApp )

doServe :: (MonadMask m, Functor m, MonadIO m) => MDB m ()
doServe = ask >>= (mkApp >=> liftIO . WARP.run 8080)

mkApp :: (MonadMask m, Functor m, MonadIO m) => MediaDb -> m Application
mkApp mdb = do
    static  <- staticFiles
    heist   <- liftIO $ getDataDir >>= \ddir -> mkHeist $ ddir ++ "/files/templates"
    return $ mapUrls $
                mount "api"     (mapUrls $
                    mount "image"   (imageApp mdb)
                <|> mount "video"   (videoApp mdb)
                <|> mountRoot (apiApp mdb)
                )
            <|> mount "static"  static
            <|> mountRoot       (templates mdb heist)

staticFiles :: MonadIO m => m Application
staticFiles = liftIO $ do
    dir <- getDataDir
    return $ staticApp (defaultFileServerSettings $ fromString $ dir ++ "/files/htdocs")

templates :: MediaDb -> HEIST.HeistState (MDB IO) -> Application
templates mdb heist _ respond = do
    let
        serveTemplate page a = do
            (hs, t) <- page heist a
            mr <- HEIST.renderTemplate hs (encodeUtf8 t)
            return $ case mr of
                Nothing -> responseLBS status404 [] BSL.empty
                Just (builder, mimeType) ->
                    responseBuilder status200 [("Content-Type", mimeType)] builder

    resp <- runMDB' mdb $ serveTemplate indexPage ()
    respond resp
