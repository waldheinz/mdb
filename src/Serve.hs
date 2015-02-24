
{-# LANGUAGE OverloadedStrings #-}

module Serve (
    doServe
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( MonadMask )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.String ( fromString )
import Network.Wai ( Application )
import Network.Wai.Application.Static ( defaultFileServerSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import Network.Wai.UrlMap ( mapUrls, mount, mountRoot )

import Mdb.Serve.Image
import Mdb.Templates
import Database
import Paths_mdb ( getDataDir )
import RestApi ( apiApp )

doServe :: (MonadMask m, Functor m, MonadIO m) => m ()
doServe = withMediaDb $ \db -> mkApp db >>= liftIO . WARP.run 8080

mkApp :: (MonadMask m, Functor m, MonadIO m) => MediaDb -> m Application
mkApp mdb = do
    static  <- staticFiles
    heist   <- liftIO $ getDataDir >>= \ddir -> mkHeist $ ddir ++ "/files/templates"
    return $ mapUrls $
                mount "api"     (apiApp mdb)
            <|> mount "image"   (imageApp mdb)
            <|> mount "static"  static
            <|> mountRoot       (templateApp mdb heist indexPage)

staticFiles :: MonadIO m => m Application
staticFiles = liftIO $ do
    dir <- getDataDir
    return $ staticApp (defaultFileServerSettings $ fromString $ dir ++ "/files/htdocs")
