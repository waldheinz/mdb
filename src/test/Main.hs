
module Main ( main ) where

import Control.Monad ( void )
import Control.Monad.IO.Class ( liftIO )
import Test.Hspec ( around, around_, describe, hspec, it )

import Mdb.Metadata as META

withMetaLookup :: (META.Lookup -> IO ()) -> IO ()
withMetaLookup f = META.mkLookup "test-data" >>= f

main :: IO ()
main = hspec $ around withMetaLookup $ do
  describe "Mdb.Metadata" $ do
    it "can be created" $ \ lup -> print lup
