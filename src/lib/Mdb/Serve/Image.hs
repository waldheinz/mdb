
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Image (
    imageApp
    ) where

import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Types     (status200)
import           Network.Wai
import           Network.Wai.Routing

import           Mdb.Database
import           Mdb.Serve.Auth         (Authenticated)
import qualified Mdb.Serve.Auth         as AUTH
import           Mdb.Serve.Utils        (withFileAccess)
import           Mdb.Types

imageApp :: MediaDb -> AUTH.SessionKey IO -> Application
imageApp mdb skey req respond = runMDB' mdb $ route root req (liftIO . respond) where
    goAuth = AUTH.request skey req
    root = prepare $
        get "/image/:id" (continue $ goAuth . getImage)        $ capture "id"

getImage :: (MonadMask m, MonadIO m) => FileId -> Authenticated m Response
getImage = withFileAccess $ \p mime ->
    return $ responseFile status200 [("Content-Type", encodeUtf8 mime)] p Nothing
