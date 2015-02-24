
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Image (
    imageApp
    ) where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Network.HTTP.Types ( status200 )
import Network.Wai
import Network.Wai.Routing

import Database
import Mdb.Database.File ( FileId, filePath )

imageApp :: MediaDb -> Application
imageApp mdb req respond = runMDB' mdb
    $ route start req (liftIO . respond)

start :: MonadIO m => Tree (App (MDB m))
start = prepare $ do
    get "/person/:id" (continue pImage)
        $ capture "id"

    get "/thumbnail/:fid" (continue pFileThumb)
        $ capture "fid"

pImage :: MonadIO m => Integer -> MDB m Response
pImage pid = personImageFile pid >>= \p -> return $ responseFile status200 [] p Nothing

pFileThumb :: MonadIO m => FileId -> MDB m Response
pFileThumb fid = do
    f <- fileById fid
    p <- fileAbs $ filePath f
    return $ responseFile status200 [] p Nothing
