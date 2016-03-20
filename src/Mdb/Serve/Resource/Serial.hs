
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Mdb.Serve.Resource.Serial (
    Serial(..), Season(..), Episode(..),
    serialResource, seasonResource, episodeResource
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Reader ( ReaderT, ask )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Except
import           Database.SQLite.Simple ( FromRow(..), field, Query )
import           Data.Aeson
import           Data.Int ( Int64 )
import           Data.JSON.Schema ( JSONSchema(..), gSchema )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Generics.Generic.Aeson
import           GHC.Generics
import           Rest
import qualified Rest.Resource as R

import           Mdb.Database
import           Mdb.Database.File ( FileId )
import           Mdb.Serve.Auth as AUTH

------------------------------------------------------------------------------------------------------------------------
-- serials
------------------------------------------------------------------------------------------------------------------------

type SerialId = Int64

type WithSerial m = ReaderT SerialId (Authenticated m)

data SerialFilter = AllSerials

data Serial = Serial
    { serialId      :: SerialId
    , serialName    :: T.Text
    , serialPoster  :: Maybe FileId
    } deriving ( Generic, Show )

instance FromRow Serial where
    fromRow = Serial <$> field <*> field <*> field

instance ToJSON Serial where
  toJSON = gtoJson

instance JSONSchema Serial where
  schema = gSchema

serialResource :: MonadIO m => Resource (Authenticated m) (WithSerial m) SerialId SerialFilter Void
serialResource = mkResourceReader
    { R.name        = "serial"
    , R.description = "Access TV serials"
    , R.schema      = withListing AllSerials $ unnamedSingle read
    , R.list        = serialList
    , R.get         = Just getSerial
    }

serialOrder :: Maybe String -> Query
serialOrder Nothing     = "series_name"
serialOrder (Just o)    = case o of
    "name"  -> "series_name"
    _       -> "series_name"

sortDir :: Maybe String -> Query
sortDir (Just "DESC")   = "DESC"
sortDir _               = "ASC"

serialList :: MonadIO m => SerialFilter -> ListHandler (Authenticated m)
serialList which = mkOrderedListing jsonO handler where
    handler :: MonadIO m => (Range, Maybe String, Maybe String) -> ExceptT Reason_ (Authenticated m) [Serial]
    handler (r, o, d) = case which of
        AllSerials  -> lift $
            AUTH.query
                (   "SELECT series_id, series_name, series_poster FROM series "
                <>  "ORDER BY " <> serialOrder o <> " " <> sortDir d <> " "
                <>  "LIMIT ? OFFSET ?" ) (count r, offset r)

getSerial :: MonadIO m => Handler (WithSerial m)
getSerial = mkIdHandler jsonO handler where
    handler :: MonadIO m => () -> SerialId -> ExceptT Reason_ (WithSerial m) Serial
    handler () sid = ExceptT $ lift $ AUTH.queryOne
        "SELECT series_id, series_name, series_poster FROM series WHERE series_id = ?" (Only sid)

------------------------------------------------------------------------------------------------------------------------
-- seasons
------------------------------------------------------------------------------------------------------------------------

type SeasonId = Int64

type WithSeason m = ReaderT SeasonId (WithSerial m)

data Season = Season
    { seasonSerialId    :: SerialId
    , seasonId          :: SeasonId
    , seasonPoster      :: Maybe FileId
    } deriving ( Generic, Show )

instance ToJSON Season where
  toJSON = gtoJson

instance JSONSchema Season where
  schema = gSchema

seasonOrder :: Maybe String -> Query
seasonOrder _     = "series_season_number"

seasonResource :: MonadIO m => Resource (WithSerial m) (WithSeason m) SeasonId () Void
seasonResource = mkResourceReader
    { R.name        = "season"
    , R.description = "Access TV serial seasons"
    , R.schema      = withListing () $ unnamedSingle read
    , R.list        = seasonList
    , R.get         = Just getSeason
    }

seasonList :: MonadIO m => () -> ListHandler (WithSerial m)
seasonList () = mkOrderedListing jsonO handler where
    handler :: MonadIO m => (Range, Maybe String, Maybe String) -> ExceptT Reason_ (WithSerial m) [Season]
    handler (r, o, d) = lift $ do
        serId  <- ask
        xs <- lift $ AUTH.query
            (   "SELECT series_season_number, series_season_poster FROM series_season "
            <>  "WHERE series_id = ? "
            <>  "ORDER BY " <> seasonOrder o <> " " <> sortDir d <> " "
            <>  "LIMIT ? OFFSET ?" ) (serId, count r, offset r)

        return $ map (uncurry (Season serId)) xs

getSeason :: MonadIO m => Handler (WithSeason m)
getSeason = mkIdHandler jsonO handler where
    handler :: MonadIO m => () -> SerialId -> ExceptT Reason_ (WithSeason m) Season
    handler () seaId = do
        serId <- lift . lift $ ask

        (Only p) <- ExceptT $ lift . lift $ AUTH.queryOne
            "SELECT series_season_poster FROM series_season WHERE series_id = ? AND series_season_number = ?"
            (serId, seaId)

        return $ Season serId seaId p

------------------------------------------------------------------------------------------------------------------------
-- episodes
------------------------------------------------------------------------------------------------------------------------

type EpisodeId = Int64

data Episode = Episode
    { episodeSerialId   :: SerialId
    , episodeSeasonId   :: SeasonId
    , episodeId         :: EpisodeId
    , episodeTitle      :: T.Text
    , episodeFile       :: Maybe FileId
    } deriving ( Generic, Show )

instance ToJSON Episode where
  toJSON = gtoJson

instance JSONSchema Episode where
  schema = gSchema

episodeOrder :: Maybe String -> Query
episodeOrder _     = "series_episode_number"

episodeResource :: MonadIO m => Resource (WithSeason m) (WithSeason m) SeasonId () Void
episodeResource = mkResourceId
    { R.name        = "episode"
    , R.description = "Access TV serial episodes"
    , R.schema      = withListing () $ named []
    , R.list        = episodeList
    }

episodeList :: MonadIO m => () -> ListHandler (WithSeason m)
episodeList () = mkOrderedListing jsonO handler where
    handler :: MonadIO m => (Range, Maybe String, Maybe String) -> ExceptT Reason_ (WithSeason m) [Episode]
    handler (r, o, d) = lift $ do
        serId    <- lift ask
        seaId    <- ask
        xs <- lift . lift $ AUTH.query
            (   "SELECT series_episode_number, series_episode_title, file_id FROM series_episode "
            <>  "WHERE series_id = ? AND series_season_number = ?"
            <>  "ORDER BY " <> episodeOrder o <> " " <> sortDir d <> " "
            <>  "LIMIT ? OFFSET ?" ) (serId, seaId, count r, offset r)

        return $ map (\(n, t, f) -> Episode serId seaId n t f) xs
