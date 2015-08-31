{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Doc where

import           Data.Aeson                       (Value (Bool), object)
import           Data.Bool                        (Bool (False, True))
import           Data.Either                      (Either (Right))
import           Data.Function                    (($))
import           Data.Maybe                       (Maybe (Just, Nothing))
import qualified Database.Couch.Explicit.Database as Database (createDoc)
import qualified Database.Couch.Explicit.Doc      as Doc (copy, delete, get,
                                                          meta, put)
import           Database.Couch.Response          (getKey)
import           Database.Couch.Types             (Context, DocRev (DocRev),
                                                   Error (Conflict, NotFound),
                                                   Result, modifyDoc,
                                                   retrieveDoc)
import           Functionality.Util               (makeTests, runTests,
                                                   testAgainstFailure,
                                                   testAgainstSchema, withDb)
import           Network.HTTP.Client              (Manager)
import           System.IO                        (IO)
import           Test.Tasty                       (TestTree)

_main :: IO ()
_main = runTests tests

tests :: IO Manager -> TestTree
tests = makeTests "Tests of the doc interface"
          [ docMeta
          , docGet
          , docPut
          , docDelete
          , docCopy
          ]

testDoc :: Value
testDoc = object [("_id", "foo"), ("llamas", Bool True)]

-- Doc-oriented functions
docMeta :: IO Context -> TestTree
docMeta =
  makeTests "Get document size and revision"
    [ withDb $ testAgainstFailure "No size information for non-existent doc" (Doc.meta retrieveDoc "foo" Nothing) NotFound
    , withDb $ testAgainstSchema
                 "Add a record and get all docs"
                 (\c -> do
                    _ :: Result Value <- Database.createDoc False testDoc c
                    Doc.meta retrieveDoc "foo" Nothing c)
                 "head--db-docid.json"
    ]

docGet :: IO Context -> TestTree
docGet =
  makeTests "Get document"
    [ withDb $ testAgainstFailure "No information for non-existent doc" (Doc.get retrieveDoc "foo" Nothing) NotFound
    , withDb $ testAgainstSchema
                 "Add a doc and get the docs"
                 (\c -> do
                    _ :: Result Value <- Database.createDoc False testDoc c
                    Doc.get retrieveDoc "foo" Nothing c)
                 "get--db-docid.json"
    ]

docPut :: IO Context -> TestTree
docPut =
  makeTests "Create and update a document"
    [ withDb $ testAgainstSchema "Simple add of document" (Doc.put modifyDoc "foo" Nothing testDoc) "put--db-docid.json"
    , withDb $ testAgainstFailure "Failure to re-add document" (\c -> do
                                                                   _ :: Result Value <- Doc.put modifyDoc "foo" Nothing testDoc c
                                                                   Doc.put modifyDoc "foo" Nothing testDoc c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then update a doc"
                 (\c -> do
                    res <- Doc.put modifyDoc "foo" Nothing testDoc c
                    let (Right (rev, _)) = getKey "rev" res
                    Doc.put modifyDoc "foo" rev testDoc c)
                 "put--db-docid.json"
    ]

docDelete :: IO Context -> TestTree
docDelete =
  makeTests "Create and update a document"
    [ withDb $ testAgainstFailure "Delete non-existent document" (Doc.delete modifyDoc "foo" Nothing) NotFound
    , withDb $ testAgainstFailure "Delete document with conflict" (\c -> do
                                                                   _ :: Result Value <- Doc.put modifyDoc "foo" Nothing testDoc c
                                                                   Doc.delete modifyDoc "foo" Nothing c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then delete doc"
                 (\c -> do
                    res <- Doc.put modifyDoc "foo" Nothing testDoc c
                    let (Right (rev, _)) = getKey "rev" res
                    Doc.delete modifyDoc "foo" rev c)
                 "delete--db-docid.json"
    ]

docCopy :: IO Context -> TestTree
docCopy =
  makeTests "Copy a document"
    [ withDb $ testAgainstFailure "Copy a non-existent document" (Doc.copy modifyDoc "foo" Nothing "bar") NotFound
    , withDb $ testAgainstFailure "Copy a document with conflict" (\c -> do
                                                                   _ :: Result Value <- Doc.put modifyDoc "foo" Nothing testDoc c
                                                                   _ :: Result Value <- Doc.put modifyDoc "bar" Nothing testDoc c
                                                                   Doc.copy modifyDoc "foo" Nothing "bar" c) Conflict
    , withDb $ testAgainstFailure
                 "Copy a document with a non-existent revision"
                 (\c -> do
                    _ :: Result Value <- Doc.put modifyDoc "foo" Nothing testDoc c
                    Doc.copy modifyDoc "foo" (Just $ DocRev "1-000000000") "bar" c)
                 NotFound
    , withDb $ testAgainstSchema
                 "Copy a document"
                 (\c -> do
                    _ :: Result Value <- Doc.put modifyDoc "foo" Nothing testDoc c
                    Doc.copy modifyDoc "foo" Nothing "bar" c)
                 "copy--db-docid.json"
    ]
