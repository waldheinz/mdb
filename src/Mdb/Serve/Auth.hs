
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Mdb.Serve.Auth (
    Authenticated, query, unsafe,
    SessionKey, request
) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import qualified Data.Vault.Lazy as V
import qualified Database.SQLite.Simple as SQL
import qualified Network.Wai as WAI
import qualified Network.Wai.Session as S

import Mdb.Database
import Mdb.Database.User

data Auth
    = NoAuth
    | UserAuth UserId

newtype Authenticated m a = Authenticated { _unAuthenticated :: ReaderT Auth (MDB m) a }
    deriving (Applicative, Functor, Monad, MonadIO )

type SessionKey m = V.Key (S.Session (MDB m) () UserId)

request :: Monad m => SessionKey m -> WAI.Request -> Authenticated m a -> MDB m a
request skey req (Authenticated f) =
    case V.lookup skey (WAI.vault req) of
        Nothing -> fail "no session storage found"
        Just (getUser, _) -> getUser () >>= \muid -> case muid of
                Nothing     -> runReaderT f NoAuth
                Just uid    -> runReaderT f (UserAuth uid)

query :: (MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Authenticated m [r]
query q p = Authenticated $ ask >>= \mauth -> case mauth of
    NoAuth      -> fail "not authorized"
    UserAuth _  -> lift $ dbQuery q p

unsafe :: Monad m => MDB m a -> Authenticated m a
unsafe f = Authenticated $ ask >>= \mauth -> case mauth of
    NoAuth      -> fail "not authorized"
    UserAuth _  -> lift f
