
module Mdb.Serve.Resource.User (
    userResource
    ) where

import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Rest ( Resource, Void, noListing, singleBy, named )
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.User ( UserId )

userResource :: MonadIO m => Resource (MDB m) (MDB m) UserId Void Void
userResource = R.mkResourceId
    { R.name    = "user"
    , R.schema  = noListing $ named [  ]
    , R.get     = Nothing
    }
