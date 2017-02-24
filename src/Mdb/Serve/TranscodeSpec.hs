
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.TranscodeSpec (
    VideoSpecH264(..), AudioSpecAAC(..), TranscodeSpec(..), Stream(..), StreamType(..), Container(..)
    ) where

import           Data.Aeson ( FromJSON(..), Value(..), (.:), eitherDecodeStrict' )
import           Data.Aeson.Types ( Parser, typeMismatch )
import           Data.Attoparsec.ByteString ( takeByteString )
import           Data.ByteString.Conversion.From (FromByteString(..))

------------------------------------------------------------------------------------------------------------------------
-- Video Streams
------------------------------------------------------------------------------------------------------------------------

data VideoSpecH264 = VideoSpecH264
    { maxRate   :: Int
    , bufSize   :: Int
    , crf       :: Int
    , width     :: Int
    } deriving ( Show )

instance FromJSON VideoSpecH264 where
    parseJSON (Object v)    = VideoSpecH264 <$> v .: "maxRate" <*> v .: "bufSize" <*> v .: "crf" <*> v .: "width"
    parseJSON x             = typeMismatch "VideoSpecH264" x

data AudioSpecAAC = AudioSpecAAC
    { bitRate   :: Int
    } deriving ( Show )

instance FromJSON AudioSpecAAC where
    parseJSON (Object v)    = AudioSpecAAC <$> v .: "bitRate"
    parseJSON x             = typeMismatch "AudioSpecAAC" x

data StreamType
    = CopyAudio
    | CopyVideo
    | TranscodeH264 VideoSpecH264
    | TranscodeAAC AudioSpecAAC
    deriving ( Show )

instance FromJSON StreamType where
    parseJSON o@(Object v) = do
        tp <- v .: "type" :: Parser String
        case tp of
            "acopy" -> return CopyAudio
            "vcopy" -> return CopyVideo
            "vh264" -> TranscodeH264 <$> parseJSON o
            "aaac"  -> TranscodeAAC <$> parseJSON o
            _       -> typeMismatch "stream type" o

    parseJSON x = typeMismatch "StreamType" x

data Stream = Stream
    { streamSource  :: Int
    , streamType    :: StreamType
    } deriving ( Show )

instance FromJSON Stream where
    parseJSON (Object v)    = Stream <$> v .: "source" <*> v .: "with"
    parseJSON x             = typeMismatch "Stream" x

data Container = Matroska | MP4
    deriving ( Show )

instance FromJSON Container where
    parseJSON (String "mkv") = pure Matroska
    parseJSON (String "mp4") = pure MP4
    parseJSON x = typeMismatch "Container" x

data TranscodeSpec = TranscodeSpec
    { container :: ! Container
    , streams   :: ! [Stream]
    } deriving ( Show )

instance FromJSON TranscodeSpec where
    parseJSON (Object v)    = TranscodeSpec <$> v .: "container" <*> v .: "streams"
    parseJSON x             = typeMismatch "TranscodeSpec" x

instance FromByteString TranscodeSpec where
    parser = fmap eitherDecodeStrict' takeByteString >>= \ej -> case ej of
        Left emsg   -> fail emsg
        Right j     -> return j
