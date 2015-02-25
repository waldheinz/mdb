
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Templates (
    mkHeist, indexPage, personPage, showPage, albumPage
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Either ( eitherT )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Heist as HEIST
import           Heist.Interpreted as HEIST

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
    <>  ("path"   HEIST.## (T.pack $ DBF.filePath f))

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
    files   <- albumFiles aid
    let spls = (bindSplice "files"  $ mapSplices file files) $ hs
    return (spls, "album")

personPage :: TemplatePage Integer
personPage hs pid = do
    p       <- getPerson pid
    albums  <- getPersonAlbums pid
    fids    <- getRandomPersonFiles pid

    let spls =
            (bindSplice "person" $ person p)
            $ (bindSplice "files"  $ mapSplices file fids)
            $ (bindSplice "albums" $ mapSplices album albums)
            $ hs

    return (spls, "person")

showPage :: TemplatePage DBF.FileId
showPage hs fid = do
    f <- fileById fid
    return ((bindSplice "file" $ file f) hs, "show-image")
