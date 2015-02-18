{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Couch.Internal (
  internalTests,
) where

import Control.Lens (
  _3,
  _4,
  _Right,
  has,
  preview,
  view,
  )
import Control.Monad (
  (>>=),
  )
import Data.Aeson.Parser (
  json,
  )
import Data.Aeson.Lens (
  _Object,
  key,
  )
import Data.Default (
  def,
  )
import Data.Function (
  ($),
  (.),
  flip,
  )
import Data.Maybe (
  Maybe (Just),
  )
import Database.Couch.Internal (
  parsedRequest,
  )
import Network.HTTP.Client (
  Manager,
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
import Test.Tasty (
  TestTree,
  testGroup,
  withResource,
  )
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  testCase,
  )

internalTests :: TestTree
internalTests =
  withResource (newManager defaultManagerSettings) closeManager allTests
  where
    allTests createManager =
      testGroup "Tests of the low-level interface" [
        checkRoot createManager
        ]

checkRoot :: IO Manager -> TestTree
checkRoot createManager = testCase "Retrieve server meta information" $ do
  res <- createManager >>= flip (parsedRequest json) def { requestHeaders = [], host = "localhost", method = "GET", path = "/", port = 5984, requestBody = RequestBodyLBS "" }
  assertBool "should have succeeded" $ has _Right res
  assertEqual "should have an empty cookie jar" def $ view (_Right._3) res
  assertBool "should have an object" $ has (_Right._4._Object) res
  assertBool "should have a couchdb key" $ has (_Right._4.key "couchdb") res
  assertEqual "should have a welcome message" (Just "Welcome") $ preview (_Right._4.key "couchdb") res
