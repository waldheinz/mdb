
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Mdb.Serve.Auth (
    Authenticated, query, queryOne, queryOneField, userExec, userExec_, unsafe, Mdb.Serve.Auth.userId,
    SessionKey, request, checkLogin, doLogout
) where

import           Control.Applicative ( (<|>) )
import           Control.Monad.Catch (MonadMask, bracket_)
import           Control.Monad.Except
import           Control.Monad.Trans.Reader ( ReaderT, ask, runReaderT )
import qualified Crypto.Scrypt as SCRYPT
import           Data.ByteString ( ByteString )
import           Data.List ( find )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Vault.Lazy as V
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.FromField as SQL
import qualified Network.Wai as WAI
import qualified Network.Wai.Session as S
-- import           Rest ( Reason(NotAllowed, NotFound) )

import Mdb.Database
import Mdb.Database.User ( UserId )

type MdbSession m = S.Session (MDB m) () UserId

data Auth m
    = TokenAuth UserId
    | UserSession (MdbSession m)

data AuthFailure
    = NoAuth
    | NoUser
    | NotFound
    deriving ( Show )

newtype Authenticated m a = Authenticated ( ExceptT AuthFailure (ReaderT (Auth m) (MDB m) ) a )
    deriving (Applicative, Functor, Monad, MonadError AuthFailure, MonadIO )

type SessionKey m = V.Key (MdbSession m)

withAuthViews :: (MonadMask m, MonadIO m) => Auth m -> MDB m a -> ExceptT AuthFailure (MDB m) a
withAuthViews (TokenAuth uid) f = lift $ withUserViews uid f
withAuthViews (UserSession (get, _)) f = (lift $ get ()) >>= maybe (throwError NoAuth) (\uid -> lift $ withUserViews uid f)

withUserViews :: (MonadMask m, MonadIO m) => UserId -> MDB m a -> MDB m a
withUserViews uid f = isolate $ bracket_ createViews destroyViews f where
    uidQuery = SQL.Query . T.pack $ show uid

    -- files
    userWhitelistTags
        =   "SELECT tag_id FROM user_tag_whitelist WHERE user_id = " <> uidQuery

    userWhitelistFiles
        =   "EXISTS ("
        <>  "   SELECT 1 FROM tag_file "
        <>  "       WHERE tag_file.file_id = f.file_id "
        <>  "       AND tag_file.tag_id IN (" <> userWhitelistTags <> ")"
        <>  ")"

    unrestricted
        =   "EXISTS ("
        <>  "   SELECT 1 FROM user"
        <>  "       WHERE user_restricted = 0"
        <>  "       AND user_id = " <> uidQuery
        <>  ")"

    authFiles
        =   "SELECT * FROM file f "
        <>  "WHERE (" <> unrestricted <> ") OR (" <> userWhitelistFiles <> ")"

    -- albums
    albumWithAuthFile
        =   "EXISTS ("
        <>  "   SELECT 1 FROM auth_file, album_file af"
        <>  "       WHERE af.album_id = a.album_id"
        <>  "       AND auth_file.file_id = af.file_id"
        <>  ")"

    authAlbums
        =   "SELECT * FROM album a "
        <>  "WHERE (" <> unrestricted <> ") OR (" <> albumWithAuthFile <> ")"

    -- persons
    personWithAuthFile
        =   "EXISTS ("
        <>  "   SELECT 1 FROM auth_file, person_file pf"
        <>  "       WHERE pf.person_id = p.person_id"
        <>  "       AND auth_file.file_id = pf.file_id"
        <>  ")"

    authPersons
        =   "SELECT * FROM person p "
        <>  "WHERE (" <> unrestricted <> ") OR (" <> personWithAuthFile <> ")"

    -- all authenticated views
    authViews =
        [ ( "auth_file"     , authFiles )
        , ( "auth_album"    , authAlbums )
        , ( "auth_person"   , authPersons )
        ]

    createViews     = mapM_ go authViews where
        go (vn, vq) = dbExecute_ $ "CREATE TEMP VIEW " <> vn <> " AS " <> vq

    destroyViews    = mapM_ (\(vn, _) -> dbExecute_ $ "DROP VIEW " <> vn) authViews

request :: (MonadMask m, MonadIO m) => SessionKey m -> WAI.Request -> Authenticated m a -> MDB m (Either T.Text a)
request skey req (Authenticated f) = runExceptT $ session where
    session = case V.lookup skey (WAI.vault req) of
        Nothing -> throwError "no session storage found"
        Just s -> do
            x <- lift $ withAuthViews (UserSession s) $ runReaderT (f) (UserSession s)
            case x of
                Left ae -> throwError "nope"
                Right x -> return x


{-
    case V.lookup skey (WAI.vault req) of
        Nothing -> fail "no session storage found"
        Just sess@(getUser, _) -> getUser () >>= \muid -> case muid of
                Just (Just uid) -> withUserViews uid $ runReaderT f (sess, UserAuth uid)
                _               -> do
                    let
                        msid = fmap snd $ find (\(pname, _) -> pname == "session_id") $ WAI.queryString req

                    case msid of
                        Nothing -> runReaderT f (sess, NoAuth)
                        Just sid -> do
                            xs <- dbQuery "SELECT user_id FROM user_session WHERE session_id=?" (Only sid)
                            case xs of
                                []              -> runReaderT f (sess, NoAuth)
                                (Only uid : _)  -> withUserViews uid $ runReaderT f (sess, UserAuth uid)

-}

query :: (MonadMask m, MonadIO m, SQL.ToRow q, SQL.FromRow r) => SQL.Query -> q -> Authenticated m [r]
query q p = unsafe $ dbQuery q p

queryOne :: (SQL.FromRow b, SQL.ToRow q, MonadMask m, MonadIO m) => SQL.Query -> q -> Authenticated m b
queryOne q p = unsafe (dbQuery q p) >>= \ xs -> case xs of
    []      -> throwError NotFound
    (a : _) -> return a

queryOneField :: (SQL.ToRow q, SQL.FromField b, MonadMask m, MonadIO m) => SQL.Query -> q -> Authenticated m b
queryOneField q p = unsafe (dbQuery q p) >>= \ xs -> case xs of
    []              -> throwError NotFound
    (Only a : _)    -> return a

userExec
    :: (MonadMask m, MonadIO m, SQL.ToRow r)
    => SQL.Query -> (UserId -> r) -> Authenticated m ()
userExec q ur = userId >>= \uid -> unsafe $ dbExecute q (ur uid)

userExec_ :: (MonadMask m, MonadIO m) => (UserId -> SQL.Query) -> Authenticated m ()
userExec_ uq = userId >>= \uid -> unsafe $ dbExecute_ (uq uid) >> return ()

userId :: Monad m => Authenticated m UserId
userId = getAuth >>= \a -> case a of
    TokenAuth uid           -> return uid
    UserSession (get, _)    -> (unsafe . get) () >>= maybe (throwError NoUser) return

checkLogin :: (MonadMask m, MonadIO m) => T.Text -> ByteString -> Authenticated m Bool
checkLogin login pass = do
    cs          <- unsafe $ dbQuery "SELECT user_pass_scrypt, user_id FROM user WHERE user_name = ?" (Only login)
    getAuth >>= \a -> case a of
        UserSession (_, put) -> do
            case cs of
                []              -> return False
                ((c, uid)  : _) -> do
                    let
                        success = SCRYPT.verifyPass' (SCRYPT.Pass pass) (SCRYPT.EncryptedPass c)

                    when success $ unsafe $ put () uid -- writes session to DB
                    return success

doLogout :: (MonadMask m, MonadIO m) => Authenticated m ()
doLogout = getAuth >>= \a -> do
    case a of
        UserSession (_, put)    -> unsafe $ put () 0
        _                       -> return ()

unsafe :: Monad m => MDB m a -> Authenticated m a
unsafe f = Authenticated $ lift . lift $ f

getAuth :: Monad m => Authenticated m (Auth m)
getAuth = Authenticated $ lift ask
