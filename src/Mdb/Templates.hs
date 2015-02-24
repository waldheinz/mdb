{-# LANGUAGE OverloadedStrings #-}

module Mdb.Templates ( mkHeist, indexPage ) where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.Trans ( lift )
import           Control.Monad.Trans.Either ( runEitherT )
import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid ( mconcat, (<>) )
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )
import           Heist as HEIST
import           Heist.Interpreted as HEIST
import           Network.HTTP.Types ( status200, status404 )
import           Network.Wai ( Application, responseBuilder, responseLBS, pathInfo )

import Database
import Mdb.Database.Person as P

personsSplice :: MonadIO m => HeistT (MDB m) (MDB m) Template
personsSplice = lift (listPersons 0 100) >>=
    mapSplices ( \p -> runChildrenWithText
        (  ("name" HEIST.## (T.pack $ P.personName p))
        <> ("id"   HEIST.## (T.pack $ show $ P.personId p))
        ))

mkHeist :: FilePath -> IO (Either [String] (HEIST.HeistState (MDB IO)))
mkHeist tmplDir = runEitherT $ do
    dis <- return $ HEIST.defaultInterpretedSplices
    lts <- return $ HEIST.defaultLoadTimeSplices

    hc <- return HEIST.emptyHeistConfig >>=
        (HEIST.hcTemplateLocations  $ \_ -> return [HEIST.loadTemplates tmplDir]) >>=
        (HEIST.hcInterpretedSplices $ \x -> return (x <> dis)) >>=
        (HEIST.hcLoadTimeSplices $ \x -> return (x <> lts))

    HEIST.initHeist hc

indexPage :: MediaDb -> HEIST.HeistState (MDB IO) -> Application
indexPage mdb hs req respond = do
    mr <- runMDB' mdb $ HEIST.renderTemplate
        (bindSplice ("persons") personsSplice hs)
        $ encodeUtf8 "index"

    case mr of
        Nothing -> respond $ responseLBS status404 [] BSL.empty
        Just (builder, mimeType) ->
            respond $ responseBuilder status200 [("Content-Type", mimeType)] builder
{-
mkTemplatesApp :: FilePath -> IO Application
mkTemplatesApp tmplDir = do
    eh <- mkHeist tmplDir

    case eh of
         Left err -> fail $ unlines err
         Right hs -> return $ \req respond -> do
            mr <- HEIST.renderTemplate hs $ encodeUtf8 . mconcat $ pathInfo req
            case mr of
                 Nothing -> do
                    mr' <- HEIST.renderTemplate hs $ encodeUtf8 . mconcat $ pathInfo req ++ ["index"]
                    case mr' of
                         Nothing -> respond $ responseLBS status404 [] BSL.empty
                         Just (builder, mimeType) ->
                            respond $ responseBuilder status200 [("Content-Type", mimeType)] builder
                            
                 Just (builder, mimeType) ->
                    respond $ responseBuilder status200 [("Content-Type", mimeType)] builder
                    -}