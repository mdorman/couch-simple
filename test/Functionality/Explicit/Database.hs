{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Database where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (return, (>>))
import           Data.Aeson                       (Object, Value (Bool, Number),
                                                   object)
import           Data.Bool                        (Bool (False, True))
import           Data.Function                    (($))
import           Data.HashMap.Strict              (lookup)
import           Data.Maybe                       (Maybe (Just))
import qualified Database.Couch.Explicit.Database as Database (allDocs,
                                                               bulkDocs,
                                                               changes, cleanup,
                                                               compact,
                                                               compactDesignDoc,
                                                               create,
                                                               createDoc,
                                                               delete, exists,
                                                               getRevsLimit,
                                                               getSecurity,
                                                               meta,
                                                               missingRevs,
                                                               purge, revsDiff,
                                                               setRevsLimit,
                                                               setSecurity,
                                                               someDocs, sync,
                                                               tempView)
import qualified Database.Couch.Response          as Response (asAnything,
                                                               asBool)
import           Database.Couch.Types             (Context (ctxDb),
                                                   DocId (DocId),
                                                   DocRev (DocRev),
                                                   DocRevMap (DocRevMap), Error (Conflict, NotFound, InvalidName, AlreadyExists),
                                                   Result, dbAllDocs,
                                                   dbBulkDocsParam,
                                                   dbChangesParam)
import           Functionality.Util               (makeTests, runTests,
                                                   testAgainstFailure,
                                                   testAgainstSchema,
                                                   testAgainstSchemaAndValue,
                                                   withDb)
import           Network.HTTP.Client              (Manager)
import           System.IO                        (IO)
import           Test.Tasty                       (TestTree)
import           Test.Tasty.HUnit                 ((@=?))

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests = makeTests "Tests of the database interface"
          [ databaseExists
          , databaseMeta
          , databaseCreate
          , databaseDelete
          , databaseCreateDoc
          , databaseAllDocs
          , databaseSomeDocs
          , databaseBulkDocs
          , databaseChanges
          , databaseCompact
          , databaseCompactDesignDoc
          , databaseSync
          , databaseCleanup
          , databaseGetSecurity
          , databaseSetSecurity
          , databaseTempView
          , databasePurge
          , databaseMissingRevs
          , databaseRevsDiff
          , databaseGetRevsLimit
          , databaseSetRevsLimit
          ]

-- Database-oriented functions
databaseExists :: IO Context -> TestTree
databaseExists = makeTests "Database existence"
                   [ testAgainstFailure "Randomly named database should not exist" Database.exists
                       NotFound
                   , withDb $ testAgainstSchema "Check for an existing database" Database.exists
                                "head--db.json"
                   ]

databaseMeta :: IO Context -> TestTree
databaseMeta =
  makeTests "Database metadata"
    [ testAgainstFailure "No metadata on non-existent database" Database.meta NotFound
    , withDb $ testAgainstSchema "Metadata for an existing database" Database.meta "get--db.json"
    ]

databaseCreate :: IO Context -> TestTree
databaseCreate =
  makeTests "Database creation"
    [ testAgainstFailure
        "Invalid name is not accepted"
        (\c -> Database.create c { ctxDb = Just "1111" })
        (InvalidName
           "Name: '1111'. Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.")
    , withDb $ testAgainstFailure "Invalid name is not accepted" Database.create AlreadyExists
    , testAgainstSchema "Result of creating a new database" (\c -> do
                                                                 createResp <- Database.create c
                                                                 _ :: Result Value <- Database.delete c
                                                                 return createResp) "put--db.json"
    ]

databaseDelete :: IO Context -> TestTree
databaseDelete =
  makeTests "Database deletion"
    [ testAgainstFailure
        "Invalid name is not accepted"
        (\c -> Database.delete c { ctxDb = Just "1111" })
        (InvalidName
           "Name: '1111'. Only lowercase characters (a-z), digits (0-9), and any of the characters _, $, (, ), +, -, and / are allowed. Must begin with a letter.")
    , testAgainstFailure "Missing name is not accepted" Database.delete NotFound
    , testAgainstSchema
        "Result of deleting a new database"
        (\c -> Response.asBool <$> Database.create c >> Database.delete c)
        "delete--db.json"
    ]

databaseCreateDoc :: IO Context -> TestTree
databaseCreateDoc =
  makeTests "Database create document"
    [ withDb $ testAgainstSchema
                 "Create a document without _id, immediate mode"
                 (Database.createDoc False (object [("llamas", Bool True)]))
                 "post--db.json"
    , withDb $ testAgainstSchema
                 "Create a document without _id, batch mode"
                 (Database.createDoc True (object [("llamas", Bool True)]))
                 "post--db.json"
    , withDb $ testAgainstSchema
                 "Create a document with _id, immediate mode"
                 (Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]))
                 "post--db.json"
    , withDb $ testAgainstSchema
                 "Create a document with _id, batch mode"
                 (Database.createDoc True (object [("_id", "foo"), ("llamas", Bool True)]))
                 "post--db.json"
    , withDb $ testAgainstFailure
                 "Try to create a document with an invalid name"
                 (Database.createDoc False (object [("_id", "_111111%%%"), ("llamas", Bool True)]))
                 (InvalidName "Only reserved document ids may start with underscore.")
    , withDb $ testAgainstFailure
                 "Try to create a document twice"
                 (\c -> do
                    _ :: Result Value <- Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                    Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c)
                 Conflict
    ]

databaseAllDocs :: IO Context -> TestTree
databaseAllDocs =
  makeTests "Database retrieve all document"
    [ withDb $ testAgainstSchema "Result of completely fresh database" (Database.allDocs dbAllDocs)
                 "get--db-_all_docs.json"
    , withDb $ testAgainstSchema
                 "Add a record and get all docs"
                 (\c -> do
                    _ :: Result Value <- Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                    Database.allDocs dbAllDocs c)
                 "get--db-_all_docs.json"
    ]

databaseSomeDocs :: IO Context -> TestTree
databaseSomeDocs =
  makeTests "Database retrieve some documents"
    [withDb $ testAgainstSchemaAndValue "Result of completely fresh database" (Database.someDocs ["llama", "tron"]) "get--db-_all_docs.json" Response.asAnything $ \step (val :: Object) -> do
      step "Check number of items in database"
      lookup "total_rows" val @=? Just (Number 0), withDb $ testAgainstSchemaAndValue
                                                              "Add a record and get all docs"
                                                              (\c -> do
                                                                 _ :: Result Value <- Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                                                                 _ :: Result Value <- Database.createDoc False (object [("_id", "bar"), ("llamas", Bool True)]) c
                                                                 Database.someDocs ["foo"] c)
                                                              "get--db-_all_docs.json"
                                                              Response.asAnything $ \step (val :: Object) -> do
                                                     step "Check number of items in database"
                                                     lookup "total_rows" val @=? Just (Number 2)]

databaseBulkDocs :: IO Context -> TestTree
databaseBulkDocs =
  makeTests "Database update documents in bulk"
    [ withDb $ testAgainstSchema "Empty list of documents" (Database.bulkDocs dbBulkDocsParam [])
      "post--db-_bulk_docs.json"
    ]

databaseChanges :: IO Context -> TestTree
databaseChanges =
  makeTests "Database update documents in bulk"
  [ withDb $ testAgainstSchema "Empty list of documents" (Database.changes dbChangesParam)
    "get--db-_changes.json"
  ]

databaseCleanup :: IO Context -> TestTree
databaseCleanup =
  makeTests "Database view cleanup"
  [ testAgainstSchema "_users database" (\c -> Database.cleanup c { ctxDb = Just "_users" }) "post--db-_view_cleanup.json"
  ]

databaseCompact :: IO Context -> TestTree
databaseCompact =
  makeTests "Database compact database"
  [ withDb $ testAgainstSchema "Empty list of documents" Database.compact "post--db-_compact.json"
  ]

databaseCompactDesignDoc :: IO Context -> TestTree
databaseCompactDesignDoc =
  makeTests "Database compact database design document"
  [ testAgainstSchema "Empty list of documents" (\c -> Database.compactDesignDoc "_auth" c { ctxDb = Just "_users" }) "post--db-_compact-ddoc.json"
  ]

databaseSync :: IO Context -> TestTree
databaseSync =
  makeTests "Database sync commit"
  [ testAgainstSchema "_users database" (\c -> Database.sync c { ctxDb = Just "_users" }) "post--db-_ensure_full_commit.json"
  ]

databaseGetSecurity :: IO Context -> TestTree
databaseGetSecurity =
  makeTests "Database get security"
  [ testAgainstSchema "_users database" (\c -> Database.getSecurity c { ctxDb = Just "_users" }) "get--db-_security.json"
  ]

databaseSetSecurity :: IO Context -> TestTree
databaseSetSecurity =
  makeTests "Database set security"
  [ withDb $ testAgainstSchema "Random database" (Database.setSecurity $ object []) "put--db-_security.json"
  ]

databaseTempView :: IO Context -> TestTree
databaseTempView =
  makeTests "Database temporary view"
  [ withDb $ testAgainstSchema "Random database" (\c -> do
                                                      _ :: Result Value <- Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                                                      Database.tempView "function (doc) { emit (1); }" (Just "_count") c) "post--db-_temp_view.json"
  ]

databasePurge :: IO Context -> TestTree
databasePurge =
  makeTests "Database purge"
  [ withDb $ testAgainstSchema "Random database" (Database.purge $ DocRevMap [(DocId "junebug", [DocRev "1-1"])]) "post--db-_purge.json"
  ]

databaseMissingRevs :: IO Context -> TestTree
databaseMissingRevs =
  makeTests "Database missing revs"
  [ withDb $ testAgainstSchema "Random database" (Database.missingRevs $ DocRevMap [(DocId "junebug", [DocRev "1-1"])]) "post--db-_missing_revs.json"
  ]

databaseRevsDiff :: IO Context -> TestTree
databaseRevsDiff =
  makeTests "Database revs diff"
  [ withDb $ testAgainstSchema "Random database" (Database.revsDiff $ DocRevMap [(DocId "junebug", [DocRev "1-1"])]) "post--db-_revs_diff.json"
  ]

databaseGetRevsLimit :: IO Context -> TestTree
databaseGetRevsLimit =
  makeTests "Database revs limit"
  [ withDb $ testAgainstSchema "Random database" Database.getRevsLimit "get--db-_revs_limit.json"
  ]

databaseSetRevsLimit :: IO Context -> TestTree
databaseSetRevsLimit =
  makeTests "Set database revs limit"
  [ withDb $ testAgainstSchema "Random database" (Database.setRevsLimit 17) "put--db-_revs_limit.json"
  ]
