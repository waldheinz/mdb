
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve (
    SessionKey, doServe
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( MonadMask )
import Control.Monad.Reader.Class ( ask )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.ByteString.Lazy as BSL
import           Data.String ( fromString )
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Vault.Lazy as V
import Network.HTTP.Types ( status200, status404 )
import           Network.Wai ( Application, responseLBS, responseBuilder )
import           Network.Wai.Application.Static ( defaultFileServerSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import           Network.Wai.Session as S
import           Network.Wai.UrlMap ( mapUrls, mount, mountRoot )
import           Heist as HEIST
import           Heist.Interpreted as HEIST
import qualified Web.Cookie as COOK

import Mdb.Serve.Auth ( SessionKey )
import Mdb.Serve.Image
import Mdb.Serve.Thumbs ( thumbApp )
import Mdb.Serve.Video
import Mdb.Templates
import Mdb.Database
import Paths_mdb ( getDataDir )
import Mdb.Serve.RestApi ( apiApp )

doServe :: (MonadMask m, MonadIO m) => MDB m ()
doServe = do
    skey <- liftIO V.newKey
    db <- ask

    let
        cname = "mdb_session"
        setc = COOK.def { COOK.setCookiePath = Just "/" }
        session sid = (getUser, setUser) where
            getUser () = do
                xs <- dbQuery "SELECT user_id FROM user_session WHERE session_id=?" (Only sid)
                case xs of
                    []              -> return Nothing
                    (Only uid : _)  -> return (Just uid)

            setUser () uid = dbExecute "INSERT INTO user_session(user_id, session_id) VALUES (?, ?)" (uid, sid)

        sstore Nothing = do
            newKey <- S.genSessionId
            return (session newKey, return newKey)
        sstore (Just sid) = return (session sid, return sid)
        sessMiddleware = S.withSession sstore cname setc skey

    app <- mkApp db skey
    liftIO $ WARP.run 8080 (sessMiddleware app)

mkApp :: (MonadMask m, Functor m, MonadIO m) => MediaDb -> SessionKey IO -> m Application
mkApp mdb skey = do
    static  <- staticFiles
    heist   <- liftIO $ getDataDir >>= \ddir -> mkHeist $ ddir ++ "/files/templates"
    return $ mapUrls $
                mount "api"     (mapUrls $
                    mount "image"   (imageApp mdb)
                <|> mount "thumb"   (thumbApp mdb skey)
                <|> mount "video"   (videoApp mdb skey)
                <|> mountRoot (apiApp mdb skey)
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
