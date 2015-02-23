
{-# LANGUAGE OverloadedStrings #-}

module Serve (
    doServe
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow, bracket )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.String ( fromString )
import Network.Wai ( Application )
import Network.Wai.Application.Static ( defaultWebAppSettings, defaultFileServerSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import Network.Wai.UrlMap ( mapUrls, mount, mountRoot )

import Mdb.Templates
import Database
import Paths_mdb ( getDataDir )
import RestApi ( apiApp )

doServe :: (MonadMask m, Functor m, MonadIO m) => m ()
doServe = mkApp >>= liftIO . WARP.run 8080

mkApp :: (MonadMask m, Functor m, MonadIO m) => m Application
mkApp = do
    static <- staticFiles
    (Just dbf) <- liftIO $ findDbFolder
    eh <- liftIO $ getDataDir >>= \ddir -> mkHeist $ ddir ++ "/files/templates"

    case eh of
         Left err       -> fail $ unlines err
         Right heist    -> return $ mapUrls $
                mount "api"     (apiApp dbf)
            <|> mount "static"  static
            <|> mountRoot       (indexPage dbf heist)

staticFiles :: MonadIO m => m Application
staticFiles = liftIO $ do
    dir <- getDataDir
    return $ staticApp (defaultFileServerSettings $ fromString $ dir ++ "/files/htdocs")
