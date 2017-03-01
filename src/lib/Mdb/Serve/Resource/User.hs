
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Mdb.Serve.Resource.User (
    LoginRequest(..), LoginResponse(..), userResource,
    VideoPlayRecord(..)
    ) where

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Error.Class ( throwError )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Except
import qualified Data.ByteString.UTF8 as BSU
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Rest
import qualified Rest.Resource as R

import           Data.Aeson
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import           Generics.Generic.Aeson
import           GHC.Generics

import           Mdb.Database (Only(..))
import           Mdb.Serve.Auth as AUTH
import           Mdb.Types

data UserSelect
    = Myself

type WithUser m = ReaderT UserSelect (Authenticated m)

userResource :: (MonadMask m, MonadIO m) => Resource (Authenticated m) (WithUser m) UserSelect Void Void
userResource = mkResourceReader
    { R.name    = "user"
    , R.schema  = noListing $ named [ ( "self", single Myself ) ]
    , R.get     = Just getUser
    , R.actions =
        [ ( "login"     , loginHandler )
        , ( "logout"    , logoutHandler )
        , ( "videoPlay" , videoPlayHandler )
        ]
    }

getUser :: (MonadMask m, MonadIO m) => Handler (WithUser m)
getUser = mkIdHandler stringO handler where
    handler :: (MonadMask m, MonadIO m) => () -> UserSelect -> ExceptT Reason_ (WithUser m) String
    handler () Myself = do
        muid <- lift . lift $ AUTH.userId
        case muid of
            Nothing     -> throwError NotAllowed
            Just uid    -> do
                (Only uname) <- ExceptT . lift $ AUTH.queryOne "SELECT user_name FROM user WHERE user_id = ?" (Only uid)
                return uname

------------------------------------------------------------------------------------------------------------------------
-- Logging in/out
------------------------------------------------------------------------------------------------------------------------

data LoginRequest = LoginRequest
    { user  :: ! T.Text
    , pass  :: ! String
    } deriving ( Generic, Show )

instance FromJSON LoginRequest where
    parseJSON = gparseJson

instance JSONSchema LoginRequest where
    schema = gSchema

data LoginResponse = LoginResponse
    { userName  :: ! T.Text
    } deriving ( Generic, Show )

instance ToJSON LoginResponse where
    toJSON = gtoJson

instance JSONSchema LoginResponse where
    schema = gSchema

loginHandler :: (MonadMask m, MonadIO m) => Handler (WithUser m)
loginHandler = mkIdHandler (jsonI . jsonO) handler where
    handler :: (MonadMask m, MonadIO m) => LoginRequest -> UserSelect -> ExceptT Reason_ (WithUser m) LoginResponse
    handler lm Myself = do
        success <- lift . lift $ AUTH.checkLogin (user lm) (BSU.fromString $ pass lm)
        if success
            then return $ LoginResponse (user lm)
            else throwError NotAllowed

logoutHandler :: (MonadMask m, MonadIO m) => Handler (WithUser m)
logoutHandler = mkIdHandler id handler where
    handler :: (MonadMask m, MonadIO m) => () -> UserSelect -> ExceptT Reason_ (WithUser m) ()
    handler () Myself = lift . lift $ AUTH.doLogout

------------------------------------------------------------------------------------------------------------------------
-- video play pos
------------------------------------------------------------------------------------------------------------------------

data VideoPlayRecord = VideoPlayRecord
    { fileId    :: FileId
    , playPos   :: Double
    } deriving ( Generic, Show )

instance FromJSON VideoPlayRecord where
    parseJSON = gparseJson

instance JSONSchema VideoPlayRecord where
    schema = gSchema

videoPlayHandler :: (MonadMask m, MonadIO m) => Handler (WithUser m)
videoPlayHandler = mkIdHandler jsonI handler where
    handler :: (MonadMask m, MonadIO m) => VideoPlayRecord -> UserSelect -> ExceptT Reason_ (WithUser m) ()
    handler vpr Myself = ExceptT . lift $ AUTH.userExec
        (   "INSERT OR REPLACE INTO user_video_play (user_id, file_id, last_play_pos, last_play_time) VALUES "
        <> "(?, ?, ?, CURRENT_TIMESTAMP)"
        )
        $ \uid -> (uid, fileId vpr, playPos vpr)
