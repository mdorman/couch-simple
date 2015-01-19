{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Couch.Internal (
  getInternalTests,
) where

import Control.Monad (
  return
  )
import Data.Aeson (
  Value (Object, String),
  )
import Data.Bool (
  Bool (False, True),
  )
import Data.Default (
  def,
  )
import Data.Either (
  Either (Right),
  )
import Data.Function (
  ($),
  )
import Data.HashMap.Strict (
  lookup,
  )
import Data.Maybe (
  maybe,
  )
import Data.Monoid (
  mempty,
  )
import Database.Couch.Internal (
  jsonRequest,
  )
import Network.HTTP.Client (
  RequestBody (RequestBodyLBS),
  closeManager,
  defaultManagerSettings,
  host,
  method,
  newManager,
  path,
  port,
  requestBody,
  requestHeaders,
  )
import System.IO (
  IO,
  )
import Test.Hspec (
  it,
  runIO,
  shouldBe,
  )
import Test.Tasty (
  TestTree,
  testGroup,
  withResource,
  )
import Test.Tasty.Hspec (
  testCase
  )

getInternalTests :: IO TestTree
getInternalTests =
  return (withResource (newManager defaultManagerSettings) (closeManager) allTests)
  where
    allTests getManager = do
      testGroup "jsonRequest tests" [
        testCase "Retrieve simple document" $ do
           let req = def { requestHeaders = [], host = "localhost", method = "GET", path = "/", port = 5984, requestBody = RequestBodyLBS "" }
           res <- runIO $ do
                  manager <- getManager
                  jsonRequest manager req return
           let success =
                 case res of
                   Right _ -> True
                   _ -> False
               value =
                 case res of
                   Right (_, _, Object o) -> maybe mempty getText $ lookup "couchdb" o
                   Right _ -> mempty
                   _ -> mempty
                 where
                   getText (String x) = x
                   getText _          = mempty
           it "should have succeeded" $
             success `shouldBe` True
           it "should have a couchdb welcome message" $
             value `shouldBe` "Welcome"
        ]
