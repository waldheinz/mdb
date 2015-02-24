
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Templates (
    mkHeist, indexPage, personPage
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Either ( eitherT )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Heist as HEIST
import           Heist.Interpreted as HEIST

import Database
import Mdb.Database.Person as P

-- personSplice :: P.Person -> HeistT (MDB m) (MDB m) Template
person p = runChildrenWithText
    (  ("name" HEIST.## (T.pack $ P.personName p))
    <> ("id"   HEIST.## (T.pack $ show $ P.personId p))
    )

--personsSplice :: MonadIO m => HeistT (MDB m) (MDB m) Template
personsSplice = lift (listPersons 0 100) >>=
    mapSplices ( \p -> (person p))

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

personPage :: TemplatePage Integer
personPage hs pid = do
    p <- getPerson pid
    return (bindSplice "person" (person p) hs, "person")
