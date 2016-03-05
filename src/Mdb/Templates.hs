
{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Mdb.Templates (
    mkHeist, indexPage, personPage, showPage,

    -- * Album related
    albumsPage, albumPage, albumShowPage
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Either ( eitherT )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Heist as HEIST
import           Heist.Interpreted as HEIST
import           Network.Wai.Predicate

import Database
import qualified Mdb.Database.File as DBF
import Mdb.Database.Album as A
import Mdb.Database.Person as P

person :: Monad n => Person -> Splice n
person p = runChildrenWithText
    (  ("name" HEIST.## (T.pack $ P.personName p))
    <> ("id"   HEIST.## (T.pack $ show $ P.personId p))
    )

album :: MonadIO m => Album -> Splice (MDB m)
album a = do

    poster <- lift $ case A.albumPoster a of
        Just p   -> return p
        Nothing  -> albumFiles (A.albumId a) >>= \fs -> case fs of
            (f:_)   -> return $ DBF.fileId f
            _       -> return 0

    runChildrenWithText
        (  ("name"      HEIST.## A.albumName a)
        <> ("id"        HEIST.## (T.pack $ show $ A.albumId a))
        <> ("poster"    HEIST.## (T.pack $ show poster))
        )

file :: Monad m => DBF.File -> Splice m
file f = runChildrenWithText $
        ("id"   HEIST.## (T.pack $ show $ DBF.fileId f))
    <>  ("path" HEIST.## (T.pack $ DBF.filePath f))

personsSplice :: MonadIO m => HeistT (MDB m) (MDB m) Template
personsSplice = lift (listPersons 0 100) >>= mapSplices ( \p -> (person p))

mkHeist :: FilePath -> IO (HEIST.HeistState (MDB IO))
mkHeist tmplDir = eitherT (fail . unlines) return $ do
    dis <- return $ HEIST.defaultInterpretedSplices
    lts <- return $ HEIST.defaultLoadTimeSplices

    hc <- return HEIST.emptyHeistConfig >>=
        (HEIST.hcTemplateLocations  $ \_ -> return [HEIST.loadTemplates tmplDir]) >>=
        (HEIST.hcInterpretedSplices $ \x -> return (x <> dis)) >>=
        (HEIST.hcLoadTimeSplices $ \x -> return (x <> lts))

    HEIST.initHeist hc

type TemplatePage a = HEIST.HeistState (MDB IO) -> a -> MDB IO ((HEIST.HeistState (MDB IO), T.Text))

indexPage :: TemplatePage a
indexPage hs _ = return (bindSplice "persons" personsSplice hs, "index")

albumPage :: TemplatePage A.AlbumId
albumPage hs aid = do

    (files, title, persons) <- withTransaction $ do
        fs <- albumFiles aid
        [(Only at)] <- dbQuery "SELECT album_name FROM album WHERE album_id = ?" (Only aid)
        ps <- dbQuery
            ( "SELECT DISTINCT p.person_id, p.person_name FROM person p "
            <>  "NATURAL JOIN person_file "
            <>  "WHERE person_file.person_id = p.person_id AND EXISTS ("
            <>      "SELECT 1 FROM album a NATURAL JOIN person_file NATURAL JOIN album_file WHERE person_file.person_id = p.person_id AND album_file.album_id = ? "
            <>  ")"
            ) (Only aid)

        return (fs, at, ps)

    let spls
            = (bindSplice "files"   $ mapSplices file files)
            $ (bindSplice "file-count"  $ textSplice (T.pack . show . length $ files))
            $ (bindSplice "name"    $ textSplice title)
            $ (bindSplice "persons" $ mapSplices person persons)
            $ hs

    return (spls, "album")

pager :: Monad n => Int -> Int -> Splice n
pager pg pgcnt = runChildrenWith ("pages" HEIST.## (mapSplices page [1..pgcnt]))
    where
        page p = runChildrenWithText
            (  ("page"  HEIST.## (T.pack $ show $ p))
            <> ("class" HEIST.## (if p == pg then "active" else ""))
            )

albumsPage :: TemplatePage Int
albumsPage hs pg = do

    (albums, pg', pgcnt) <- withTransaction $ do
        [(Only acnt)] <- dbQuery_ "SELECT COUNT(1) from album"

        let
            perPage = 6 * 4
            pages   = (acnt + perPage - 1) `div` perPage
            pg'     = max 1 $ min pages pg
            off     = perPage * (pg' - 1)

        as <- getAlbums off perPage
        return $ (as, pg', pages)

    let spls
            = bindSplice "albums" (mapSplices album albums)
            $ bindSplice "pagination" (pager pg' pgcnt)
            $ hs

    return  (spls, "page-albums")

-- | show a file in the context of an album
albumShowPage :: TemplatePage (Integer ::: Integer)
albumShowPage hs (_ ::: fid) = do

    showPage hs fid

personPage :: TemplatePage Integer
personPage hs pid = do
    p       <- getPerson pid
    albums  <- getPersonAlbums pid
    fids    <- getRandomPersonFiles pid

    let spls
            = (bindSplice "person" $ person p)
            $ (bindSplice "files"  $ mapSplices file fids)
            $ (bindSplice "albums" $ mapSplices album albums)
            $ hs

    return (spls, "person")

showPage :: TemplatePage DBF.FileId
showPage hs fid = fileById fid >>= \f -> case T.takeWhile ( /= '/') (DBF.fileMime f) of
    "image" -> return ((bindSplice "file" $ file f) hs, "show-image")
    "video" -> return ((bindSplice "file" $ file f) hs, "show-video")
    _       -> fail $ "unsupported MIME type " ++ (T.unpack $ DBF.fileMime f)
