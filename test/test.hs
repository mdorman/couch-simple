module Main where

import Test.Tasty

main :: IO ()
main = do
  let allTests = testGroup "All tests" []
  defaultMain allTests
