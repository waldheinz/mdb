
{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Mdb.Serve.Thumbs (
    thumbApp
    ) where

import           Control.Monad.Catch             (MonadMask)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.Text.Encoding              (encodeUtf8Builder)
import           Network.HTTP.Types              (status200, status400)
import           Network.Wai
import           Network.Wai.Predicate
import           Network.Wai.Routing

import           Mdb.Database
import           Mdb.Image                       (ThumbSize(..), ensureThumb)
import           Mdb.Serve.Auth                  as AUTH
import           Mdb.Serve.Utils                 (withFileAccess)
import           Mdb.Types

thumbApp :: MediaDb -> AUTH.SessionKey IO -> Application
thumbApp mdb skey req respond = runMDB' mdb $ route root req (liftIO . respond) where
    goAuth = AUTH.request skey req
    root = prepare $
        get "/:size/:fid" (continue $ goAuth . fileThumb) $ capture "size" .&. capture "fid"

fileThumb :: (MonadMask m, MonadIO m) => ThumbSize ::: FileId -> Authenticated m Response
fileThumb (ts ::: fid) = withFileAccess go fid where
    go filePath fileMime = AUTH.unsafe $ do
        etf <- runExceptT $ ensureThumb ts fid filePath fileMime
        case etf of
            Left msg -> return $ responseBuilder status400
                [ ( "Content-Type", "text/plain; charset=utf8")
                ] (encodeUtf8Builder msg)
            Right thumbFile -> return $ responseFile status200
                [ ("Cache-Control", "max-age=3600")
                , ("Content-Type", "image/jpeg")
                ] thumbFile Nothing
