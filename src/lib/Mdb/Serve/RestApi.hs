
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.RestApi (
    apiApp, api
    ) where

import qualified Network.Wai as WAI
import           Rest.Driver.Wai ( apiToApplication )
import           Rest.Api ( Some1(..), route, root, (-/), (--/), (---/), Api(Unversioned) )

import           Mdb.Database
import           Mdb.Serve.Auth as AUTH
import           Mdb.Serve.Resource.Album ( albumResource, personAlbumResource )
import           Mdb.Serve.Resource.File ( fileResource )
import           Mdb.Serve.Resource.Person ( personResource )
import           Mdb.Serve.Resource.Serial ( serialResource, seasonResource, episodeResource )
import           Mdb.Serve.Resource.User ( userResource )

apiApp :: MediaDb -> AUTH.SessionKey IO -> WAI.Application
apiApp mdb skey req = apiToApplication (runMDB' mdb . AUTH.request skey req) api req

api :: Api (Authenticated IO)
api = Unversioned $ Some1 $ root
            -/ albums
            -/ files
            -/ persons
                --/ route personAlbumResource
            -/ serial
                --/ route seasonResource
                    ---/ route episodeResource
            -/ user

    where
        albums      = route albumResource
        files       = route fileResource
        persons     = route personResource
        serial      = route serialResource
        user        = route userResource
