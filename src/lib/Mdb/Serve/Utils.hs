
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Serve.Utils (
    withFileAccess
    ) where

import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text as T
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Types     (status404)
import           Network.Wai

import           Mdb.Database
import           Mdb.Serve.Auth         (Authenticated)
import qualified Mdb.Serve.Auth         as AUTH
import           Mdb.Types

-- |
-- Tries to locate a file by id in the auth_file table and if found provides the absolute path to that file
-- and the mime type to the provided continuation. Otherwise a 404 response is generated.
withFileAccess
    :: (MonadIO m, MonadMask m)
    => (String -> T.Text -> Authenticated m Response)
    -> FileId -> Authenticated m Response
withFileAccess f fid = do
    mf <- AUTH.queryOne "SELECT file_name, file_mime FROM auth_file WHERE file_id=?" (Only fid)
    case mf of
        Left _ -> return $ responseLBS status404
            [ ("Content-Type", "text/plain; charset=utf-8")
            ] $ BSL.fromChunks [ encodeUtf8 "it's not there" ]
        Right (filePath, fileMime) -> do
            fsPath <- AUTH.unsafe . fileAbs $ filePath
            f fsPath fileMime
