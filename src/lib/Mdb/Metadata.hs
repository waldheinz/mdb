
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Metadata (
  Lookup, mkLookup,

  -- ** Query
  ignoreFile
  ) where

import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Logger ( logWarnN )
import qualified Data.Cache as C
import           Data.Maybe ( catMaybes )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Data.Yaml ( FromJSON(..), (.:) )
import           System.FilePath ( takeDirectory )
import qualified System.FilePath.Glob as GLOB

import           Mdb.Database ( MDB )

data FolderMetaYaml = FolderMetaYaml
    { yamlIgnoreGlobs :: ! [String]
    }

instance FromJSON FolderMetaYaml where
    parseJSON = Y.withObject "folder meta" $ \v -> FolderMetaYaml <$> v .: "ignore"

data FolderMeta = FolderMeta
    { metaIgnoreGlobs :: ! [GLOB.Pattern]
    }

resolveMeta :: Applicative m => FolderMetaYaml -> MDB m FolderMeta
resolveMeta yaml = pure $ FolderMeta (map GLOB.compile $ yamlIgnoreGlobs yaml)

data Lookup = Lookup
  { basePath  :: ! FilePath
  , yamlCache :: ! (C.Cache FilePath FolderMeta)
  }

instance Show Lookup where
  show lup = "Metadata Lookup [base=" ++ basePath lup ++ "]"

class HasLookup m where
    getLookup :: m Lookup

mkLookup :: MonadIO m => FilePath -> m Lookup
mkLookup base = liftIO $ Lookup base <$> C.newCache (Just 5)

folderMeta :: MonadIO m => FilePath -> MDB m (Maybe FolderMetaYaml)
folderMeta f = (liftIO . Y.decodeFileEither) f >>= either (\e -> logError e >> return Nothing) (return . Just)
    where
        logError e = logWarnN $ "error parsing " <> T.pack f <> ": " <> (T.pack . show) e

metasFor :: MonadIO m => FilePath -> MDB m [FolderMeta]
metasFor f = catMaybes <$> mapM go parents where
    go d = folderMeta d >>= maybe (return Nothing) (\m -> resolveMeta m >>= return . Just)
    parents = takeWhile (/= ".") $ iterate takeDirectory f

ignoreFile :: MonadIO m => FilePath -> m Bool
ignoreFile = undefined
