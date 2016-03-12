
{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Mdb.Templates (
    mkHeist, indexPage
    ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Either ( eitherT )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Heist as HEIST
import           Heist.Interpreted as HEIST

import Mdb.Database
import Mdb.Database.Person as P

person :: Monad n => Person -> Splice n
person p = runChildrenWithText
    (  ("name" HEIST.## (T.pack $ P.personName p))
    <> ("id"   HEIST.## (T.pack $ show $ P.personId p))
    )
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
