{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Couch.Explicit.Configuration (
  configurationTests,
) where

import Control.Lens (
  _1,
  _Right,
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
  AsPrimitive,
  _String,
  key,
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
import qualified Database.Couch.Explicit.Configuration as Configuration (
  delValue,
  getValue,
  section,
  server,
  setValue,
  )
import Database.Couch.Types (
  Context (Context),
  CouchError (..),
  Port (Port),
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

-- Configuration-oriented functions
configurationTests :: TestTree
configurationTests =
  withResource createContext releaseContext allTests
  where
    createContext =
      newManager defaultManagerSettings >>= \manager -> return $ Context manager "localhost" (Port 5984) Nothing def Nothing
    releaseContext =
      closeManager . ctxManager
    allTests getContext =
      testGroup "Tests of the configuration interface" [
        section getContext,
        server getContext,
        setValue getContext,
        getValue getContext,
        delValue getContext
        ]

delValue :: IO Context -> TestTree
delValue getContext = testCase "Delete configured value" $ do
  res <- getContext >>= Configuration.delValue "key" "subkey"
  valueTests res

getValue :: IO Context -> TestTree
getValue getContext = testCase "Retrieve configured value" $ do
  res <- getContext >>= Configuration.getValue "key" "subkey"
  valueTests res

server :: IO Context -> TestTree
server getContext = testCase "Retrieve top-level configuration" $ do
  res <- getContext >>= Configuration.server
  basicSuccess res
  checkObject res
  assertBool "should have a couchdb key" $ has (_Right._1.key "couchdb") res

section :: IO Context -> TestTree
section getContext = testCase "Retrieve section configuration" $ do
  res <- getContext >>= Configuration.section "couchdb"
  basicSuccess res
  checkObject res
  assertBool "should have a database_dir key" $ has (_Right._1.key "database_dir") res

setValue :: IO Context -> TestTree
setValue getContext = testCase "Set a configuration value" $ do
  res <- getContext >>= Configuration.setValue "key" "subkey" "value"
  assertBool "should return a string" $ has (_Right._1._String) res
  assertEqual "should have a value of the empty string" (Just "") $ preview (_Right._1._String) res

valueTests :: AsPrimitive a => Either CouchError (a, Maybe CookieJar) -> IO ()
valueTests res = do
  basicSuccess res
  assertBool "should return a string" $ has (_Right._1._String) res
  assertEqual "should have a value of value" (Just "value") $ preview (_Right._1._String) res
