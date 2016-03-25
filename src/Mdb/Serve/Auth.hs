
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Mdb.Serve.Auth (
    Authenticated, query, queryOne, unsafe, Mdb.Serve.Auth.userId,
    SessionKey, request, checkLogin
) where

import           Control.Monad.Catch (MonadMask, bracket_)
import           Control.Monad.Except
import           Control.Monad.Trans.Reader ( ReaderT, asks, runReaderT )
import qualified Crypto.Scrypt as SCRYPT
import           Data.ByteString ( ByteString )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import qualified Database.SQLite.Simple as SQL
import qualified Network.Wai as WAI
import qualified Network.Wai.Session as S
import           Rest ( Reason(NotAllowed, NotFound) )

import Mdb.Database
import Mdb.Database.User

data Auth
    = NoAuth
    | UserAuth ! UserId

newtype Authenticated m a = Authenticated ( ReaderT (S.Session (MDB m) () UserId, Auth) (MDB m) a )
    deriving (Applicative, Functor, Monad, MonadIO )

type SessionKey m = V.Key (S.Session (MDB m) () UserId)

request :: (MonadMask m, MonadIO m) => SessionKey m -> WAI.Request -> Authenticated m a -> MDB m a
request skey req (Authenticated f) =
    case V.lookup skey (WAI.vault req) of
        Nothing -> fail "no session storage found"
        Just sess@(getUser, _) -> getUser () >>= \muid -> case muid of
                Nothing     -> runReaderT f (sess, NoAuth)
                Just uid    -> do
                    let
                        userWhitelist
                            =   "SELECT tag_id FROM user_tag_whitelist WHERE user_id = "
                            <>  (SQL.Query . T.pack $ show uid)

                        createViews = dbExecute_
                                (   "CREATE TEMP VIEW auth_file AS "
                                <>  "SELECT * FROM file WHERE EXISTS "
                                <>  "   (SELECT * FROM tag_file "
                                <>  "       WHERE tag_file.file_id = file.file_id "
                                <>  "           AND tag_file.tag_id IN (" <> userWhitelist <> ")"
                                <>  "   )"
                                )

                        destroyViews = dbExecute_ "DROP VIEW auth_file"

                    isolate $ bracket_ createViews destroyViews $! runReaderT f (sess, UserAuth uid)

query :: (MonadMask m, MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Authenticated m [r]
query q p = Authenticated $ asks snd >>= \mauth -> case mauth of
    NoAuth          -> fail "not authorized"
    UserAuth _      -> lift $! dbQuery q p

queryOne :: (SQL.FromRow b, SQL.ToRow q, MonadMask m, MonadIO m) => SQL.Query -> q -> Authenticated m (Either (Reason a) b)
queryOne q p = Authenticated $ asks snd >>= \mauth -> case mauth of
    NoAuth      -> return $ Left NotAllowed
    UserAuth _  -> lift (dbQuery q p) >>= \ xs -> return $! case xs of
                        []      -> Left NotFound
                        (a : _) -> Right a

unsafe :: Monad m => MDB m a -> Authenticated m a
unsafe f = Authenticated $ asks snd >>= \mauth -> case mauth of
    NoAuth      -> fail "not authorized"
    UserAuth _  -> lift $! f

userId :: Monad m => Authenticated m (Maybe UserId)
userId = Authenticated $ asks snd >>= \a -> case a of
    UserAuth uid    -> return $ Just uid
    _               -> return Nothing

checkLogin :: (MonadMask m, MonadIO m) => String -> ByteString -> Authenticated m Bool
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
