
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Video (
    videoApp
    ) where

import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Network.HTTP.Types ( status200 )
import           Network.Wai
import           Network.Wai.Routing

import           Database
import qualified Mdb.Database.File as DBF

videoApp :: MediaDb -> Application
videoApp mdb req respond = runMDB' mdb $ route root req (liftIO . respond) where
    root = prepare $
        get "/streamDirect/:id" (continue getStream)      $ capture "id"

getStream :: MonadIO m => DBF.FileId -> MDB m Response
getStream fid = do
    f <- fileById fid
    p <- fileAbs $ DBF.filePath f
    return $ responseFile status200 [] p Nothing
