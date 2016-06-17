
{-# LANGUAGE OverloadedStrings, TypeOperators #-}

module Mdb.Templates (
    mkHeist, indexPage
    ) where

import           Control.Monad.Trans.Either ( eitherT )
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Heist as HEIST

import Mdb.Database

mkHeist :: FilePath -> IO (HEIST.HeistState (MDB IO))
mkHeist tmplDir = eitherT (fail . unlines) return $ do
    let
        dis = HEIST.defaultInterpretedSplices
        lts = HEIST.defaultLoadTimeSplices

    hc <- return HEIST.emptyHeistConfig >>=
        (HEIST.hcTemplateLocations  $ \_ -> return [HEIST.loadTemplates tmplDir]) >>=
        (HEIST.hcInterpretedSplices $ \x -> return (x <> dis)) >>=
        (HEIST.hcNamespace (\_ -> return "")) >>=
        (HEIST.hcLoadTimeSplices $ \x -> return (x <> lts))

    HEIST.initHeist hc

type TemplatePage a = HEIST.HeistState (MDB IO) -> a -> MDB IO (HEIST.HeistState (MDB IO), T.Text)

indexPage :: TemplatePage a
indexPage hs _ = return (hs, "index")
