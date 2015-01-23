module Main where

import Test.Tasty
import Couch.Internal
import Couch.Explicit

main :: IO ()
main = do
  let allTests = testGroup "All tests" [internalTests, explicitTests]
  defaultMain allTests
