
{-# LANGUAGE OverloadedStrings #-}

module Mdb.Templates ( mkHeist, templateApp, indexPage ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Either ( eitherT )
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid ( (<>) )
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )
import           Heist as HEIST
import           Heist.Interpreted as HEIST
import           Network.HTTP.Types ( status200, status404 )
import           Network.Wai ( Application, responseBuilder, responseLBS )

import Database
import Mdb.Database.Person as P

personsSplice :: MonadIO m => HeistT (MDB m) (MDB m) Template
personsSplice = lift (listPersons 0 100) >>=
    mapSplices ( \p -> runChildrenWithText
        (  ("name" HEIST.## (T.pack $ P.personName p))
        <> ("id"   HEIST.## (T.pack $ show $ P.personId p))
        ))

mkHeist :: FilePath -> IO (HEIST.HeistState (MDB IO))
mkHeist tmplDir = eitherT (fail . unlines) return $ do
    dis <- return $ HEIST.defaultInterpretedSplices
    lts <- return $ HEIST.defaultLoadTimeSplices

    hc <- return HEIST.emptyHeistConfig >>=
        (HEIST.hcTemplateLocations  $ \_ -> return [HEIST.loadTemplates tmplDir]) >>=
        (HEIST.hcInterpretedSplices $ \x -> return (x <> dis)) >>=
        (HEIST.hcLoadTimeSplices $ \x -> return (x <> lts))

    HEIST.initHeist hc

type TemplatePage = HEIST.HeistState (MDB IO) -> (HEIST.HeistState (MDB IO), T.Text)

templateApp :: MediaDb -> HEIST.HeistState (MDB IO) -> TemplatePage -> Application
templateApp mdb hs page _ respond = do
    let
        (hs', t) = page hs

    mr <- runMDB' mdb $ HEIST.renderTemplate hs' $ encodeUtf8 t

    case mr of
        Nothing -> respond $ responseLBS status404 [] BSL.empty
        Just (builder, mimeType) ->
            respond $ responseBuilder status200 [("Content-Type", mimeType)] builder

indexPage :: TemplatePage
indexPage hs = (bindSplice ("persons") personsSplice hs, "index")
