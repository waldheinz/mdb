
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Container ( containerResource ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Except ( ExceptT(..) )
import           Rest
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.Container ( Container )
import           Mdb.Database.File ( FileId )
import           Mdb.Serve.Auth as AUTH
import           Mdb.Serve.Resource.File ( WithFile )

containerResource :: MonadIO m => Resource (WithFile m) (WithFile m) FileId Void Void
containerResource = mkResourceId
    { R.name    = "container"
    , R.schema  = noListing $ named [ ]
    , R.get     = Just getContainer
    }

getContainer :: MonadIO m => Handler (WithFile m)
getContainer = mkIdHandler jsonO handler where
    handler :: MonadIO m => () -> FileId -> ExceptT Reason_ (WithFile m) Container
    handler () fid = ExceptT $ lift $ AUTH.queryOne
        "SELECT file_id, container_duration, container_format FROM container WHERE file_id=?" (Only fid)
