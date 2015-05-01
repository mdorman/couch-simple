{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Explicit.Server where

import           Control.Applicative            ((<$>))
import           Control.Monad                  ((>>=))
import           Data.Either                    (Either (Left, Right))
import           Data.Foldable                  (for_)
import           Data.Function                  (($), (.))
import           Data.List                      (length)
import           Data.UUID                      (toString)
import qualified Database.Couch.Explicit.Server as Server (activeTasks, allDbs,
                                                           meta, restart, stats,
                                                           uuids)
import           Database.Couch.Response        as Response (asUUID)
import           Database.Couch.Types           (Context)
import           Functionality.Util             (checkCookiesAndSchema,
                                                 runTests, serverContext,
                                                 testAgainstSchema)
import           Network.HTTP.Client            (Manager)
import           System.IO                      (IO)
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (assertFailure, testCaseSteps,
                                                 (@=?))
import           Text.Show                      (show)

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests manager = testGroup "Tests of the server interface" $
  ($ serverContext manager) <$> [serverMeta, activeTasks, allDbs, stats, uuids]

-- Server-oriented functions
serverMeta :: IO Context -> TestTree
serverMeta = testAgainstSchema "Get server meta information" Server.meta "get--.json"

activeTasks :: IO Context -> TestTree
activeTasks = testAgainstSchema "Get list of active tasks" Server.activeTasks "get--_active_tasks.json"

allDbs :: IO Context -> TestTree
allDbs = testAgainstSchema "Retrieve list of all dbs (should be empty)" Server.allDbs "get--_all_dbs.json"

-- This fails when run alone, so it's commented out; I need to have it
-- able to run some other thing concurrently to provoke some actual
-- content
-- dbUpdates :: IO Context -> TestTree
-- dbUpdates getContext = testCaseSteps "Retrieve list of database updates" $ do
--   res <- getContext >>= Server.dbUpdates
--   checkRequestSuccess res
--   assertBool "should have an array of objects" $ allOf (_Right._1.each) (has _Object) res
--   assertBool "should have pids for all tasks" $ allOf (_Right._1.each) (has (key "pid")) res
--   checkEmptyCookieJar res

restart :: IO Context -> TestTree
restart = testAgainstSchema "Restart server" Server.restart "post--_restart.json"

stats :: IO Context -> TestTree
stats = testAgainstSchema "Retrieve statistics" Server.stats "get--_stats.json"

uuids :: IO Context -> TestTree
uuids getContext = testCaseSteps "Retrieve UUIDs" $ \step -> do
  step "Make request"
  res <- getContext >>= Server.uuids 1
  checkCookiesAndSchema step "get--_uuids.json" res
  case Response.asUUID res of
    Left error -> assertFailure (show error)
    Right (values, _) -> do
      step "Check length of list"
      length values @=? 1
      for_ values $ \u -> (length . toString) u @=? 36
