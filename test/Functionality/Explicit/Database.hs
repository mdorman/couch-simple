{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Explicit.Database where

import           Control.Applicative              ((<$>))
import           Data.Function                    (($))
import           Data.Functor                     (fmap)
import           Data.Maybe                       (Maybe (Just))
import qualified Database.Couch.Explicit.Database as Database (create, exists,
                                                               meta)
import           Database.Couch.Types             (Context (ctxDb),
                                                   CouchError (..))
import           Functionality.Util               (dbContext, runTests,
                                                   testAgainstFailure,
                                                   testAgainstSchema, withDb)
import           Network.HTTP.Client              (Manager)
import           System.IO                        (IO)
import           Test.Tasty                       (TestTree, testGroup)

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests manager = testGroup "Tests of the database interface" $
  ($ dbContext manager) <$> [databaseExists, databaseMeta, databaseCreate]

-- Database-oriented functions
databaseExists :: IO Context -> TestTree
databaseExists getContext =
  testGroup "Database existence"
    [
      testAgainstFailure "Randomly named database should not exist" Database.exists NotFound getContext,
      withDb getContext $ testAgainstSchema "Check for an existing database" Database.exists "head--db.json"
    ]

databaseMeta :: IO Context -> TestTree
databaseMeta getContext =
  testGroup "Database metadata"
  [
      testAgainstFailure "No metadata on non-existent database" Database.meta NotFound getContext,
      withDb getContext $ testAgainstSchema "Metadata for an existing database" Database.meta "get--db.json"
  ]

databaseCreate :: IO Context -> TestTree
databaseCreate getContext =
  testGroup "Database creation"
  [
    testAgainstFailure "Invalid name is not accepted" Database.create (InvalidName "Name: '1111'. Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.") (fmap (\c -> c { ctxDb = Just "1111" }) getContext),
    withDb getContext $ testAgainstFailure "Invalid name is not accepted" Database.create AlreadyExists,
    testAgainstSchema "Result of creating a new database" Database.create "put--db.json" getContext
  ]
