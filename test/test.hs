module Main where

import Test.Tasty
import Couch.Internal

main :: IO ()
main = do
  internalTests <- getInternalTests
  let allTests = testGroup "All tests" [internalTests]
  defaultMain allTests
