
module Mdb.Metadata (
  Lookup, mkLookup
  ) where

import qualified Data.Cache as C

data FolderMeta

data Lookup = Lookup
  { basePath  :: ! FilePath
  , yamlCache :: ! (C.Cache FilePath FolderMeta)
  }

instance Show Lookup where
  show lup = "Metadata Lookup [base=" ++ basePath lup ++ "]"

mkLookup :: FilePath -> IO Lookup
mkLookup base = Lookup base <$> C.newCache (Just 5)
