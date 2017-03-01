
module Main ( main ) where

import qualified Rest.Gen as RG
import qualified Rest.Gen.Config as RGC
import qualified Mdb.Serve.RestApi as API

main :: IO ()
main = do
  config <- RGC.configFromArgs "rest-example-gen"
  RG.generate config "RestExample" API.api [] [] []
