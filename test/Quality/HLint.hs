{-# LANGUAGE NoImplicitPrelude #-}

module Quality.HLint where

import           Data.Function          (($))
import           Language.Haskell.HLint (hlint)
import           System.IO              (IO)
import           Test.Tasty             (TestTree, defaultMain)
import           Test.Tasty.HUnit       (testCase, (@=?))

-- For interactive testing
_main :: IO ()
_main = defaultMain tests

-- Run hlint over source and tests alike
tests :: TestTree
tests = testCase "HLint" $ do
  hints <- hlint ["--quiet", "src", "test"]
  [] @=? hints
