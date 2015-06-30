{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Database where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (void, (>>))
import           Data.Aeson                       (Object, Value (Bool, Number),
                                                   object)
import           Data.Bool                        (Bool (False, True))
import           Data.Function                    (($))
import           Data.Functor                     (fmap)
import           Data.HashMap.Strict              (lookup)
import           Data.Maybe                       (Maybe (Just))
import qualified Database.Couch.Explicit.Database as Database (allDocs, create,
                                                               createDoc,
                                                               delete, exists,
                                                               meta, someDocs)
import qualified Database.Couch.Response          as Response (asAnything,
                                                               asBool)
import           Database.Couch.Types             (Context (ctxDb),
                                                   CouchError (..), dbAllDocs)
import           Functionality.Util               (dbContext, runTests,
                                                   testAgainstFailure,
                                                   testAgainstSchema,
                                                   testAgainstSchemaAndValue,
                                                   withDb)
import           Network.HTTP.Client              (Manager)
import           System.IO                        (IO)
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.HUnit                 ((@=?))

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests manager = testGroup "Tests of the database interface" $
  ($ dbContext manager) <$> [databaseExists, databaseMeta, databaseCreate, databaseDelete, databaseCreateDoc, databaseAllDocs, databaseSomeDocs]

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

databaseDelete :: IO Context -> TestTree
databaseDelete getContext =
  testGroup "Database deletion"
  [
    testAgainstFailure "Invalid name is not accepted" Database.delete (InvalidName "Name: '1111'. Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.") (fmap (\c -> c { ctxDb = Just "1111" }) getContext),
    testAgainstFailure "Missing name is not accepted" Database.delete NotFound getContext,
    testAgainstSchema "Result of deleting a new database"  (\c -> Response.asBool <$> Database.create c >> Database.delete c) "delete--db.json" getContext
  ]

databaseCreateDoc :: IO Context -> TestTree
databaseCreateDoc getContext =
  testGroup "Database create document"
  [
      withDb getContext $ testAgainstSchema "Create a document without _id, immediate mode" (Database.createDoc False (object [("llamas", Bool True)])) "post--db.json",
      withDb getContext $ testAgainstSchema "Create a document without _id, batch mode" (Database.createDoc True (object [("llamas", Bool True)])) "post--db.json",
      withDb getContext $ testAgainstSchema "Create a document with _id, immediate mode" (Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)])) "post--db.json",
      withDb getContext $ testAgainstSchema "Create a document with _id, batch mode" (Database.createDoc True (object [("_id", "foo"), ("llamas", Bool True)])) "post--db.json",
      withDb getContext $ testAgainstFailure "Try to create a document with an invalid name" (Database.createDoc False (object [("_id", "_111111%%%"), ("llamas", Bool True)])) (InvalidName "Only reserved document ids may start with underscore."),
      withDb getContext $ testAgainstFailure "Try to create a document twice" (\c -> do
                                                                                   void $ Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                                                                                   Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c) Conflict
  ]

databaseAllDocs :: IO Context -> TestTree
databaseAllDocs getContext =
  testGroup "Database retrieve all document"
  [
    withDb getContext $ testAgainstSchema "Result of completely fresh database"  (Database.allDocs dbAllDocs) "get--db-_all_docs.json",
    withDb getContext $ testAgainstSchema "Add a record and get all docs"  (\c -> do
                                                                                      void $ Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                                                                                      Database.allDocs dbAllDocs c) "get--db-_all_docs.json"
  ]

databaseSomeDocs :: IO Context -> TestTree
databaseSomeDocs getContext =
  testGroup "Database retrieve some documents"
  [
    withDb getContext $ testAgainstSchemaAndValue "Result of completely fresh database" (Database.someDocs ["llama", "tron"]) "get--db-_all_docs.json" Response.asAnything $ \step (val :: Object) -> do
        step "Check number of items in database"
        lookup "total_rows" val @=? Just (Number 0),
    withDb getContext $ testAgainstSchemaAndValue "Add a record and get all docs"  (\c -> do
                                                                                        void $ Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                                                                                        void $ Database.createDoc False (object [("_id", "bar"), ("llamas", Bool True)]) c
                                                                                        Database.someDocs ["foo"] c) "get--db-_all_docs.json" Response.asAnything $ \step (val :: Object) -> do
      step "Check number of items in database"
      lookup "total_rows" val @=? Just (Number 2)
 ]
