{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Couch.Explicit.Database where

import Control.Lens (
  _1,
  _Left,
  _Right,
  at,
  has,
  makePrisms,
  preview,
  set,
  )
import Control.Monad (
  (>>=),
  liftM,
  return,
  )
import Control.Monad.IO.Class (
  liftIO,
  )
import Couch.Util (
  basicSuccess,
  checkBooleanResult,
  checkObject,
  checkRequestFailure,
  )
import Data.Aeson (
  Value (Number, Object, String),
  )
import Data.Aeson.Lens (
  _Object,
  key,
  )
import Data.Bool (
  Bool (False, True),
  )
import Data.Default (
  def,
  )
import Data.Either (
  Either (Left, Right),
  )
import Data.Function (
  ($),
  (.),
  )
import Data.HashMap.Strict (
  fromList,
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
import Data.UUID (
  toString,
  )
import qualified Database.Couch.Explicit.Database as Database (
  allDocs,
  create,
  createDoc,
  delete,
  exists,
  meta,
  )
import Database.Couch.Types (
  Context (Context),
  CouchError (..),
  CreateResult (..),
  DocId (DocId),
  Port (Port),
  ctxDb,
  ctxManager,
  dbAllDocs,
  )
import Network.HTTP.Client (
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
  assertBool,
  assertEqual,
  assertFailure,
  testCase,
  )
import Text.Show (
  show,
  )

makePrisms ''CouchError
makePrisms ''CreateResult

databaseTests :: TestTree
databaseTests =
  withResource createContext releaseContext allTests
  where
    createContext = do
      uuid <- liftM (fromString . ("aaa" <>) . toString) (liftIO randomIO)
      manager <- newManager defaultManagerSettings
      return $ Context manager "localhost" (Port 5984) Nothing def (Just uuid)
    releaseContext =
      closeManager . ctxManager
    allTests getContext =
      testGroup "Tests of the database interface" [
        databaseExists getContext,
        databaseMeta getContext,
        databaseCreate getContext,
        databaseDocuments getContext
        -- databaseDelete getContext
        ]

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

baseDoc :: Value
baseDoc = Object (fromList [("foo",String "bar")])

databaseDocuments :: IO Context -> TestTree
databaseDocuments getContext = testGroup "Document handling"
                               [testCase "Create document in sync mode" $ do
                                   let localId = "YourHighness"
                                       doc = set (_Object.at "_id") (Just . String $ localId) baseDoc
                                   res <- getContext >>= Database.createDoc False doc
                                   case res of
                                     Left error -> assertFailure (show error)
                                     Right (val, cj) -> do
                                       assertEqual "Check that cookie jar is empty" cj Nothing
                                       case val of
                                         NoRev docId -> assertFailure ("Result should have rev: " <> show docId)
                                         WithRev docId _ -> assertEqual "Check DocId is correct" (DocId localId) docId,
                                testCase "Create document in batch mode" $ do
                                   let localId = "YourLowness"
                                       doc = set (_Object.at "_id") (Just . String $ localId) baseDoc
                                   res <- getContext >>= Database.createDoc True doc
                                   case res of
                                     Left error -> assertFailure (show error)
                                     Right (val, cj) -> do
                                       assertEqual "Check that cookie jar is empty" cj Nothing
                                       case val of
                                         NoRev docId -> assertEqual "Check DocId is correct" (DocId localId) docId
                                         WithRev docId _ -> assertFailure ("Result should not have rev: " <> show docId),
                                testCase "Retrieve all documents" $ do
                                   res <- getContext >>= Database.allDocs dbAllDocs
                                   case res of
                                     Left error -> assertFailure (show error)
                                     Right (val, cj) -> do
                                       assertEqual "Check that cookie jar is empty" cj Nothing
                                       case val of
                                         Object _ -> do
                                           assertEqual "Has offset" (preview (key "offset") val) (Just . Number $ 0)
                                           assertEqual "Has offset" (preview (key "total_rows") val) (Just . Number $ 2)
                                         _ -> assertFailure ("Result should have been an object: " <> show val)
                                  ]
