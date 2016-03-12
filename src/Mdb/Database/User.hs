
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Mdb.Database.User (
    UserId, User(..)
    ) where

import           Database.SQLite.Simple ( FromRow(..), field )
import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import qualified Data.Text as T
import           Generics.Generic.Aeson
import           GHC.Generics

type UserId = Int64

data User = User
    { userId            :: ! UserId
    , userName          :: ! T.Text
    , userPassScrypt    :: ! T.Text
    } deriving ( Generic, Show )

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

instance ToJSON User where
  toJSON = gtoJson

instance FromJSON User where
  parseJSON = gparseJson

instance JSONSchema User where
  schema = gSchema
