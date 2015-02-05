{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Couch.Explicit (
  explicitTests,
) where

import Control.Concurrent (
  threadDelay,
  )
import Control.Lens (
  _1,
  _2,
  _Left,
  _Nothing,
  _Right,
  allOf,
  each,
  has,
  makePrisms,
  preview,
  )
import Control.Monad (
  (>>=),
  liftM,
  return,
  )
import Control.Monad.IO.Class (
  MonadIO,
  liftIO,
  )
import Data.Aeson (
  Value,
  )
import Data.Aeson.Lens (
  _Object,
  key,
  )
import Data.Bool (
  Bool (False, True),
  not,
  )
import Data.Default (
  def,
  )
import Data.Either (
  Either,
  )
import Data.Function (
  ($),
  (.),
  )
import Data.Maybe (
  Maybe (Just, Nothing),
  )
import Data.Monoid (
  (<>),
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
import qualified Database.Couch.Explicit.Database as Database (
  create,
  delete,
  exists,
  meta,
  )
import qualified Database.Couch.Explicit.Server as Server (
  activeTasks,
  allDbs,
  -- dbUpdates,
  meta,
  restart,
  stats,
  uuids,
  )
import Database.Couch.Types (
  Context (Context),
  CouchError (..),
  Port (Port),
  ctxDb,
  ctxManager,
  )
import Network.HTTP.Client (
  CookieJar,
  closeManager,
  defaultManagerSettings,
  newManager,
  )
import System.IO (
  IO,
  )
import System.Random (
  randomIO,
  )
import Test.Tasty (
  TestTree,
  testGroup,
  withResource,
  )
import Test.Tasty.HUnit (
  Assertion,
  assertBool,
  assertEqual,
  testCase,
  )

makePrisms ''CouchError

explicitTests :: TestTree
explicitTests = testGroup "Tests of explicit interface"
  [ serverTests
  , databaseTests
  , restartTest
  ]

databaseTests :: TestTree
databaseTests =
  withResource createContext releaseContext allTests
  where
    createContext = do
      uuid <-liftM (fromString . ("aaa" <>) . toString) (liftIO randomIO)
      manager <- newManager defaultManagerSettings
      return $ Context manager "localhost" (Port 5984) Nothing def (Just uuid)
    releaseContext =
      closeManager . ctxManager
    allTests getContext =
      testGroup "Tests of the database interface" [
        databaseExists getContext,
        databaseMeta getContext,
        databaseCreate getContext,
        databaseDelete getContext
        ]

restartTest :: TestTree
restartTest =
  withResource createContext releaseContext restart
  where
    createContext =
      newManager defaultManagerSettings >>= \manager -> return $ Context manager "localhost" (Port 5984) Nothing def Nothing
    releaseContext =
      closeManager . ctxManager

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

checkBooleanResult :: Bool -> Either CouchError (Bool, Maybe CookieJar) -> Assertion
checkBooleanResult b res = assertEqual "should have a true value" (Just b) $ preview (_Right._1) res

checkEmptyCookieJar :: Either CouchError (a, Maybe CookieJar) -> Assertion
checkEmptyCookieJar res = assertBool "should have an empty cookie jar" (has (_Right._2._Nothing) res)

checkObject :: Either CouchError (Value, Maybe CookieJar) -> Assertion
checkObject res = assertBool "should have an object" $ has (_Right._1._Object) res

checkRequestFailure :: Either CouchError a -> Assertion
checkRequestFailure res = assertBool "should have failed" $ has _Left res

checkRequestSuccess :: Either CouchError a -> Assertion
checkRequestSuccess res = assertBool "should have succeeded" $ has _Right res

basicSuccess :: Either CouchError (a, Maybe CookieJar) -> Assertion
basicSuccess res = do
  checkEmptyCookieJar res
  checkRequestSuccess res

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

restart :: IO Context -> TestTree
restart getContext = testCase "Restart the server" $ do
  res <- getContext >>= Server.restart
  basicSuccess res
  checkBooleanResult True res
  threadDelay 1000000

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

-- Database-oriented functions
databaseExists :: IO Context -> TestTree
databaseExists getContext = testGroup "Database existence"
                            [testCase "Check for a non-existent database" $ do
                                res <- getContext >>= \c -> Database.exists c { ctxDb = Just "shouldnotexist" }
                                basicSuccess res
                                checkBooleanResult False res
                            ,testCase "Check for an existing database" $ do
                                res <- getContext >>= \c -> Database.exists c { ctxDb = Just "_users" }
                                basicSuccess res
                                checkBooleanResult True res]

databaseMeta :: IO Context -> TestTree
databaseMeta getContext = testCase "Retrieve database meta information (use always-present _users)" $ do
  res <- getContext >>= \c -> Database.meta c { ctxDb = Just "_users" }
  basicSuccess res
  checkObject res
  assertBool "should have a db_name key" $ has (_Right._1.key "db_name") res
  assertEqual "should have its name in the key" (Just "_users") $ preview (_Right._1.key "db_name") res

databaseCreate :: IO Context -> TestTree
databaseCreate getContext = testGroup "Database existence"
                            [testCase "Try to use an invalid name" $ do
                                res <- getContext >>= \c -> Database.create c { ctxDb = Just "1111" }
                                checkRequestFailure res
                                assertBool "should have an invalid field name error" $ has (_Left._InvalidName) res
                            ,testCase "Try to create an existing database" $ do
                                res <- getContext >>= \c -> Database.create c { ctxDb = Just "_users" }
                                checkRequestFailure res
                                assertBool "should have an invalid field name error" $ has (_Left._AlreadyExists) res
                            ,testCase "Create database" $ do
                                res <- getContext >>= Database.create
                                basicSuccess res
                                checkBooleanResult True res]

-- FIXME: Need to test database creation without privileges

databaseDelete :: IO Context -> TestTree
databaseDelete getContext = testGroup "Database deletion"
                            [testCase "Try to delete an invalid name" $ do
                                res <- getContext >>= \c -> Database.delete c { ctxDb = Just "1111" }
                                checkRequestFailure res
                                assertBool "should have an invalid field name error" $ has (_Left._InvalidName) res
                            ,testCase "Delete test database" $ do
                                res <- getContext >>= Database.delete
                                basicSuccess res
                                checkBooleanResult True res
                            ,testCase "Delete test database again" $ do
                                res <- getContext >>= Database.delete
                                checkRequestFailure res
                                assertBool "should have an invalid field name error" $ has (_Left._NotFound) res]

-- FIXME: Need to test database deletion without privileges
