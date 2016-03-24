
module Mdb.Types (
    FileId,
    PersonId,
    AlbumId,
    SerialId, SeasonId, EpisodeId,
    TagId
    ) where

import           Data.Int               (Int64)

type FileId = Int64

type AlbumId = Int64

type PersonId = Int64

type SerialId = Int64
type SeasonId = Int64
type EpisodeId = Int64

type TagId = Int64
