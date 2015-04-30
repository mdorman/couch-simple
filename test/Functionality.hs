{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative    ((<$>))
import           Data.Function          (($))
import qualified Functionality.Internal as Internal (tests)
import           Functionality.Util     (runTests)
import           Network.HTTP.Client    (Manager)
import           System.IO              (IO)
import           Test.Tasty             (TestTree, testGroup)

main :: IO ()
main = runTests tests

tests :: Manager -> TestTree
tests manager = testGroup "All Tests" $
                  ($ manager) <$> [Internal.tests]
