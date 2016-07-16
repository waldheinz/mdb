
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
import           Network.Wai ( Application )
import           Network.Wai.Application.Static ( defaultFileServerSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import           Network.Wai.Handler.WebSockets ( websocketsOr )
import           Network.Wai.Session as S
import           Network.Wai.UrlMap ( mapUrls, mount, mountRoot )
import qualified Network.WebSockets as WS
import qualified Web.Cookie as COOK

import Mdb.Serve.Auth ( SessionKey )
import Mdb.Serve.Image
import Mdb.Serve.Thumbs ( thumbApp )
import Mdb.Serve.Video
import Mdb.Serve.VideoWs ( videoWsApp )
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
    liftIO $ WARP.run 8080 $ mapUrls $
        mount "videows" (websocketsOr WS.defaultConnectionOptions videoWsApp undefined) <|>
        mountRoot (sessMiddleware app)

mkApp :: (MonadMask m, MonadIO m) => MediaDb -> SessionKey IO -> m Application
mkApp mdb skey = do
    static  <- staticFiles
    return $ mapUrls $
                mount "api"     (mapUrls $
                    mount "image"   (imageApp mdb skey)
                <|> mount "thumb"   (thumbApp mdb skey)
                <|> mount "video"   (videoApp mdb skey)
                <|> mountRoot (apiApp mdb skey)
                )
            <|> mountRoot static

staticFiles :: MonadIO m => m Application
staticFiles = liftIO $ do
    dir <- getDataDir
    return $ staticApp (defaultFileServerSettings $ fromString $ dir ++ "/files/htdocs")
