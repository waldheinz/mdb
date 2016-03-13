
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Mdb.Serve.Auth (
    Authenticated, query, unsafe, Mdb.Serve.Auth.userId,
    SessionKey, request, checkLogin
) where

import           Control.Monad ( when )
import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.Reader ( ReaderT, asks, runReaderT )
import qualified Crypto.Scrypt as SCRYPT
import           Data.ByteString ( ByteString )
import qualified Data.Vault.Lazy as V
import qualified Database.SQLite.Simple as SQL
import qualified Network.Wai as WAI
import qualified Network.Wai.Session as S

import Mdb.Database
import Mdb.Database.User

data Auth
    = NoAuth
    | UserAuth UserId

newtype Authenticated m a = Authenticated { _unAuthenticated :: ReaderT (S.Session (MDB m) () UserId, Auth) (MDB m) a }
    deriving (Applicative, Functor, Monad, MonadIO )

type SessionKey m = V.Key (S.Session (MDB m) () UserId)

request :: Monad m => SessionKey m -> WAI.Request -> Authenticated m a -> MDB m a
request skey req (Authenticated f) =
    case V.lookup skey (WAI.vault req) of
        Nothing -> fail "no session storage found"
        Just sess@(getUser, _) -> getUser () >>= \muid -> case muid of
                Nothing     -> runReaderT f (sess, NoAuth)
                Just uid    -> runReaderT f (sess, UserAuth uid)

query :: (MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Authenticated m [r]
query q p = Authenticated $ asks snd >>= \mauth -> case mauth of
    NoAuth      -> fail "not authorized"
    UserAuth _  -> lift $ dbQuery q p

unsafe :: Monad m => MDB m a -> Authenticated m a
unsafe f = Authenticated $ asks snd >>= \mauth -> case mauth of
    NoAuth      -> fail "not authorized"
    UserAuth _  -> lift f

userId :: Monad m => Authenticated m (Maybe UserId)
userId = Authenticated $ asks snd >>= \a -> case a of
    UserAuth uid    -> return $ Just uid
    _               -> return Nothing

checkLogin :: MonadIO m => String -> ByteString -> Authenticated m Bool
checkLogin login pass = Authenticated $ do
    cs      <- lift $ dbQuery "SELECT user_pass_scrypt, user_id FROM user WHERE user_name = ?" (Only login)
    (_, put)    <- asks fst

    case cs of
        []              -> return False
        ((c, uid)  : _)    -> do
            let
                success = SCRYPT.verifyPass' (SCRYPT.Pass pass) (SCRYPT.EncryptedPass c)

            when success $ lift $ put () uid -- writes session to DB
            return success
