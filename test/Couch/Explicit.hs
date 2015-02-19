{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Couch.Explicit (
  explicitTests,
) where

import Control.Concurrent (
  threadDelay,
  )
import Control.Monad (
  (>>=),
  return,
  )
import Couch.Explicit.Configuration (
  configurationTests,
  )
import Couch.Explicit.Database (
  databaseTests,
  )
import Couch.Explicit.Server (
  serverTests,
  )
import Couch.Util (
  basicSuccess,
  checkBooleanResult,
  )
import Data.Bool (
  Bool (True),
  )
import Data.Default (
  def,
  )
import Data.Function (
  ($),
  (.),
  )
import Data.Maybe (
  Maybe (Nothing),
  )
import qualified Database.Couch.Explicit.Server as Server (
  restart,
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
import Test.Tasty (
  TestTree,
  testGroup,
  withResource,
  )
import Test.Tasty.HUnit (
  testCase,
  )

explicitTests :: TestTree
explicitTests = testGroup "Tests of explicit interface"
  [ serverTests
  , configurationTests
  , databaseTests
  , restartTest
  ]

restartTest :: TestTree
restartTest =
  withResource createContext releaseContext restart
  where
    createContext =
      newManager defaultManagerSettings >>= \manager -> return $ Context manager "localhost" (Port 5984) Nothing def Nothing
    releaseContext =
      closeManager . ctxManager
    restart getContext = testCase "Restart the server" $ do
      res <- getContext >>= Server.restart
      basicSuccess res
      checkBooleanResult True res
      threadDelay 1000000
