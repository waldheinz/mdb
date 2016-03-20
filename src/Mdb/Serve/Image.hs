
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Image (
    imageApp
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Network.HTTP.Types     (status200)
import           Network.Wai
import           Network.Wai.Routing

import           Mdb.Database
import qualified Mdb.Database.File      as DBF
import           Mdb.Types

imageApp :: MediaDb -> Application
imageApp mdb req respond = runMDB' mdb
    $ route start req (liftIO . respond)

start :: MonadIO m => Tree (App (MDB m))
start = prepare $
    get "/image/:id" (continue getImage)        $ capture "id"

getImage :: MonadIO m => FileId -> MDB m Response
getImage fid = do
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f
    return $ responseFile status200 [] p Nothing
