
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.RestApi (
    apiApp
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import qualified Network.Wai as WAI
import           Rest.Driver.Wai ( apiToApplication )
import           Rest.Api ( Router, Some1(..), route, root, mkVersion, (-/), (--/) )

import           Mdb.Database
import           Mdb.Serve.Auth as AUTH
import           Mdb.Serve.Resource.Album ( albumResource, personAlbumResource )
import           Mdb.Serve.Resource.File ( fileResource )
import           Mdb.Serve.Resource.Person ( personResource )
import           Mdb.Serve.Resource.User ( userResource )
import           Mdb.Serve.Resource.Video ( videoResource )

apiApp :: MediaDb -> AUTH.SessionKey IO -> WAI.Application
apiApp mdb skey req = apiToApplication (runMDB' mdb . AUTH.request skey req) api req
    where
        api = [(mkVersion 0 1 0, Some1 api010)]

api010 :: (Applicative m, MonadIO m) => Router (Authenticated m) (Authenticated m)
api010 = root
            -/ albums
            -/ files
                --/ video
            -/ persons
                --/ route personAlbumResource
            -/ user

    where
        albums  = route albumResource
        files   = route fileResource
        persons = route personResource
        user    = route userResource
        video   = route videoResource
