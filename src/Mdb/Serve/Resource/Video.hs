
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Resource.Video ( videoResource ) where

import           Control.Monad ( liftM )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT )
import           Control.Monad.Trans.Class ( lift )
import           Rest
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.File ( FileId )
import           Mdb.Database.Video ( VideoId, Video )
import           Mdb.Serve.Auth as AUTH
import           Mdb.Serve.Resource.File ( WithFile )

type WithVideo m = ReaderT VideoId (WithFile m)

videoResource :: MonadIO m => Resource (WithFile m) (WithVideo m) VideoId Void Void
videoResource = mkResourceReader
    { R.name    = "video"
    , R.schema  = noListing $ named [ ]
    , R.get     = Just getVideo
    }

getVideo :: MonadIO m => Handler (WithVideo m)
getVideo = mkIdHandler jsonO $ \() fid -> lift . lift . lift $ getVideoForFile fid

getVideoForFile :: MonadIO m => FileId -> Authenticated m Video
getVideoForFile fid = liftM head $ AUTH.query
    "SELECT video_id, video_duration, video_format FROM video WHERE file_id=?"
    (Only fid)
