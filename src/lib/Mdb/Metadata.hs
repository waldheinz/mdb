
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Metadata (
  Lookup, mkLookup
  ) where

import qualified Data.Cache as C
import qualified Data.Yaml as Y
import           Data.Yaml ( FromJSON(..), (.:) )
import qualified System.FilePath.Glob as GLOB

data FolderMetaYaml = FolderMetaYaml
    { yamlIgnoreGlobs :: [String]
    }

instance FromJSON FolderMetaYaml where
    parseJSON = Y.withObject "folder meta" $ \v -> FolderMetaYaml <$> v .: "ignore"

data FolderMeta

data Lookup = Lookup
  { basePath  :: ! FilePath
  , yamlCache :: ! (C.Cache FilePath FolderMeta)
  }

instance Show Lookup where
  show lup = "Metadata Lookup [base=" ++ basePath lup ++ "]"

mkLookup :: FilePath -> IO Lookup
mkLookup base = Lookup base <$> C.newCache (Just 5)
