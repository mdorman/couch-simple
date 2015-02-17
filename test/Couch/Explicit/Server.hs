{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Couch.Explicit.Server where

import Control.Lens (
  _1,
  _Right,
  allOf,
  each,
  has,
  preview,
  )
import Control.Monad (
  (>>=),
  return,
  )
import Couch.Util (
  basicSuccess,
  checkObject,
  )
import Data.Aeson.Lens (
  _Object,
  key,
  )
import Data.Bool (
  not,
  )
import Data.Default (
  def,
  )
import Data.Function (
  ($),
  (.),
  )
import Data.Maybe (
  Maybe (Just, Nothing),
  )
import Data.String (
  fromString,
  )
import Data.Text (
  null,
  )
import Data.UUID (
  toString,
  )
import qualified Database.Couch.Explicit.Server as Server (
  activeTasks,
  allDbs,
  -- dbUpdates,
  meta,
  stats,
  uuids,
  )
import Database.Couch.Types (
  Context (Context),
  Port (Port),
  ctxManager,
  )
import Network.HTTP.Client (
  closeManager,
  defaultManagerSettings,
  newManager,
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

serverTests :: TestTree
serverTests =
  withResource createContext releaseContext allTests
  where
    createContext =
      newManager defaultManagerSettings >>= \manager -> return $ Context manager "localhost" (Port 5984) Nothing def Nothing
    releaseContext =
      closeManager . ctxManager
    allTests getContext =
      testGroup "Tests of the server interface" [
        serverMeta getContext,
        activeTasks getContext,
        allDbs getContext,
        -- dbUpdates getContext,
        stats getContext,
        uuids getContext
        ]

-- Server-oriented functions
serverMeta :: IO Context -> TestTree
serverMeta getContext = testCase "Retrieve server meta information" $ do
  res <- getContext >>= Server.meta
  basicSuccess res
  checkObject res
  assertBool "should have a couchdb key" $ has (_Right._1.key "couchdb") res
  assertEqual "should have a welcome message" (Just "Welcome") $ preview (_Right._1.key "couchdb") res

activeTasks :: IO Context -> TestTree
activeTasks getContext = testCase "Retrieve list of active tasks" $ do
  res <- getContext >>= Server.activeTasks
  basicSuccess res
  assertBool "should have an array of objects" $ allOf (_Right._1.each) (has _Object) res
  assertBool "should have pids for all tasks" $ allOf (_Right._1.each) (has (key "pid")) res

allDbs :: IO Context -> TestTree
allDbs getContext = testCase "Retrieve list of all dbs (should be empty)" $ do
  res <- getContext >>= Server.allDbs
  basicSuccess res
  assertBool "should have an array of non-empty names" $ allOf (_Right._1.each) (not . null) res

-- This fails when run alone, so it's commented out; I need to have it
-- able to run some other thing concurrently to provoke some actual
-- content
-- dbUpdates :: IO Context -> TestTree
-- dbUpdates getContext = testCase "Retrieve list of database updates" $ do
--   res <- getContext >>= Server.dbUpdates
--   checkRequestSuccess res
--   assertBool "should have an array of objects" $ allOf (_Right._1.each) (has _Object) res
--   assertBool "should have pids for all tasks" $ allOf (_Right._1.each) (has (key "pid")) res
--   checkEmptyCookieJar res

stats :: IO Context -> TestTree
stats getContext = testCase "Retrieve statistics" $ do
  res <- getContext >>= Server.stats
  basicSuccess res
  checkObject res
  assertBool "should have a top-level couchdb entry" $ has (_Right._1.key "couchdb") res
  assertBool "should have a top-level httpd_request_methods entry" $ has (_Right._1.key "httpd_request_methods") res
  assertBool "should have a top-level httpd_status_codes entry" $ has (_Right._1.key "httpd_status_codes") res
  assertBool "should have a top-level httpd entry" $ has (_Right._1.key "httpd") res

uuids :: IO Context -> TestTree
uuids getContext = testCase "Retrieve UUIDs" $ do
  res <- getContext >>= Server.uuids 1
  basicSuccess res
  assertBool "should have an array of non-empty names" $ allOf (_Right._1.each) (not . null . fromString . toString) res
