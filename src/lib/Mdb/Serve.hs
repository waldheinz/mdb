
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve (
    SessionKey, doServe
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad.Catch ( MonadMask )
import Control.Monad.Reader.Class ( ask )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.String ( fromString )
import qualified Data.Vault.Lazy as V
import           Network.HTTP.Types.Status ( status200 )
import           Network.Wai ( Application, responseFile )
import           Network.Wai.Application.Static ( defaultWebAppSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import           Network.Wai.Session as S
import           Network.Wai.UrlMap ( mapUrls, mount, mountRoot )
import qualified Web.Cookie as COOK

import Mdb.Serve.Auth ( SessionKey )
import Mdb.Serve.Image
import Mdb.Serve.Thumbs ( thumbApp )
import Mdb.Serve.Video
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

            setUser () (Just uid)   = dbExecute
                "INSERT INTO user_session(user_id, session_id) VALUES (?, ?)" (uid, sid)

            setUser () Nothing      = dbExecute
                "DELETE FROM user_session WHERE session_id = ?" (Only sid)

        sstore Nothing = do
            newKey <- S.genSessionId
            return (session newKey, return newKey)

        sstore (Just sid) = return (session sid, return sid)
        sessMiddleware = S.withSession sstore cname setc skey

    app <- mkApp db skey
    liftIO $ WARP.run 8080 $ sessMiddleware app

mkApp :: (MonadMask m, MonadIO m) => MediaDb -> SessionKey IO -> m Application
mkApp mdb skey = do
    res <- mkResources
    index <- mkIndex

    return $ mapUrls $
            mount "api"     (mapUrls $
                mount "image"   (imageApp mdb skey)
            <|> mount "thumb"   (thumbApp mdb skey)
            <|> mount "video"   (videoApp mdb skey)
            <|> mountRoot (apiApp mdb skey)
            )
        <|> mount "res" res
        <|> mountRoot   index

mkResources :: MonadIO m => m Application
mkResources = liftIO $ do
    dir <- getDataDir
    return $ staticApp (defaultWebAppSettings $ fromString $ dir ++ "/files/htdocs/res")

mkIndex :: MonadIO m => m Application
mkIndex = liftIO $ do
    dir <- getDataDir
    return $ \_ respond ->
        respond $ responseFile status200 [ ] (fromString $ dir ++ "/files/htdocs/index.html") Nothing
