
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Mdb.Serve.Auth (
    Authenticated, query, unsafe,
    request
) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import qualified Database.SQLite.Simple as SQL
import qualified Network.Wai as WAI

import Mdb.Database
import Mdb.Database.User

data Auth
    = NoAuth
    | UserAuth UserId

newtype Authenticated m a = Authenticated { _unAuthenticated :: ReaderT Auth (MDB m) a }
    deriving (Applicative, Functor, Monad, MonadIO )

request :: Authenticated m a -> MDB m a
request (Authenticated f) = do
    runReaderT f NoAuth

query :: (MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Authenticated m [r]
query q p = Authenticated $ lift $ dbQuery q p

unsafe :: Monad m => MDB m a -> Authenticated m a
unsafe = Authenticated . lift
