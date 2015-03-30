
{-# LANGUAGE OverloadedStrings #-}

module Serve (
    doServe
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( MonadMask )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy as BSL
import Data.String ( fromString )
import           Data.Text.Encoding ( encodeUtf8 )
import Network.HTTP.Types ( status200, status404 )
import Network.Wai ( Application, responseLBS, responseBuilder )
import Network.Wai.Application.Static ( defaultFileServerSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import Network.Wai.UrlMap ( mapUrls, mount, mountRoot )
import Network.Wai.Predicate
import Network.Wai.Routing
import           Heist as HEIST
import           Heist.Interpreted as HEIST

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
            <|> mountRoot       (templates mdb heist)

staticFiles :: MonadIO m => m Application
staticFiles = liftIO $ do
    dir <- getDataDir
    return $ staticApp (defaultFileServerSettings $ fromString $ dir ++ "/files/htdocs")

templates :: MediaDb -> HEIST.HeistState (MDB IO) -> Application
templates mdb heist req respond = do
    runMDB' mdb $ route (start heist) req (liftIO . respond)

start :: HEIST.HeistState (MDB IO) -> Tree (App (MDB IO))
start heist = prepare $ do
    let
        tpl = continue . serveTemplate
        serveTemplate page a = do
            (hs, t) <- page heist a
            mr <- HEIST.renderTemplate hs (encodeUtf8 t)
            return $ case mr of
                Nothing -> responseLBS status404 [] BSL.empty
                Just (builder, mimeType) ->
                    responseBuilder status200 [("Content-Type", mimeType)] builder

    get "/"                     (tpl indexPage)     $ true
    get "/albums"               (tpl albumsPage)    $ constant 1
    get "/albums/:pg"           (tpl albumsPage)    $ capture "pg"
    get "/album/:aid"           (tpl albumPage)     $ capture "aid"
    get "/album/:aid/show/:fid" (tpl albumShowPage) $ capture "aid" .&. capture "fid"
    get "/person/:id"           (tpl personPage)    $ capture "id"
    get "/show/:fid"            (tpl showPage)      $ capture "fid"
