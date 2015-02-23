
module Serve (
    doServe
    ) where

import Control.Applicative ( (<|>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.String ( fromString )
import Network.Wai ( Application )
import Network.Wai.Application.Static ( defaultWebAppSettings, defaultFileServerSettings, staticApp )
import qualified Network.Wai.Handler.Warp as WARP
import Network.Wai.UrlMap ( mapUrls, mountRoot )

import Database
import Paths_mdb ( getDataDir )

doServe :: MonadIO m => MDB m ()
doServe = app >>= liftIO . WARP.run 8080

app :: MonadIO m => m Application
app = do
    static <- staticFiles
    return $ mapUrls $
        mountRoot static

staticFiles :: MonadIO m => m Application
staticFiles = liftIO $ do
    dir <- getDataDir
    return $ staticApp (defaultFileServerSettings $ fromString $ dir ++ "/files/htdocs")
