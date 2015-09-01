{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified Functionality (tests)
import qualified Quality       (tests)
import           System.IO     (IO)
import           Test.Tasty    (TestTree, defaultMain, testGroup)

-- Run all teests by default
main :: IO ()
main = defaultMain tests

-- Everything we can throw at it
tests :: TestTree
tests = testGroup "All tests" [
  Functionality.tests,
  Quality.tests
  ]
