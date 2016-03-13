
{-# LANGUAGE DeriveGeneric #-}

module Mdb.Serve.Resource.User (
    userResource
    ) where

import           Control.Monad.Error.Class ( throwError )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT, ask )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Except
import qualified Data.ByteString.UTF8 as BSU
import qualified Network.Wai as WAI
import           Rest
import qualified Rest.Resource as R

import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import qualified Data.Text as T
import           Generics.Generic.Aeson
import           GHC.Generics


import           Mdb.Database.User ( UserId )
import           Mdb.Serve.Auth as AUTH

data UserSelect
    = Myself

type WithUser m = ReaderT UserSelect (Authenticated m)

userResource :: MonadIO m => Resource (Authenticated m) (WithUser m) UserSelect Void Void
userResource = mkResourceReader
    { R.name    = "user"
    , R.schema  = noListing $ named [ ( "self", single Myself ) ]
    , R.get     = Just getUser
    , R.actions = [ ( "login", loginHandler ) ]
    }

getUser :: MonadIO m => Handler (WithUser m)
getUser = mkIdHandler stringO handler where
    handler :: MonadIO m => () -> UserSelect -> ExceptT Reason_ (WithUser m) String
    handler () Myself = do
        muid <- lift . lift $ AUTH.userId
        case muid of
            Nothing     -> throwError NotAllowed
            Just uid    -> return "jupp"

------------------------------------------------------------------------------------------------------------------------
-- Logging in
------------------------------------------------------------------------------------------------------------------------

data LoginMessage = LoginMessage
    { user  :: String
    , pass  :: String
    } deriving ( Generic, Show )

instance FromJSON LoginMessage where
    parseJSON = gparseJson

instance JSONSchema LoginMessage where
    schema = gSchema

loginHandler :: MonadIO m => Handler (WithUser m)
loginHandler = mkIdHandler (jsonI . stringO) handler where
    handler :: MonadIO m => LoginMessage -> UserSelect -> ExceptT Reason_ (WithUser m) String
    handler lm Myself = do
        success <- lift . lift $ AUTH.checkLogin (user lm) (BSU.fromString $ pass lm)
        if success
            then return (user lm)
            else throwError NotAllowed
