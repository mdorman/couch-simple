{-# LANGUAGE NoImplicitPrelude #-}

module Quality where

import qualified Quality.HLint as HLint (tests)
import           System.IO     (IO)
import           Test.Tasty    (TestTree, defaultMain, testGroup)

-- For interactive testing
_main :: IO ()
_main = defaultMain tests

-- Tests for code quality rather than functionality
tests :: TestTree
tests = testGroup "Code Quality Tests" [
  HLint.tests
  ]
