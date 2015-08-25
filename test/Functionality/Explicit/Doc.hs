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
                                                          put, size)
import           Database.Couch.Response          (getKey)
import           Database.Couch.Types             (Context, DocRev (DocRev),
                                                   Error (Conflict, NotFound),
                                                   Result, docGetDoc,
                                                   docPutParam)
import           Functionality.Util               (makeTests, runTests,
                                                   testAgainstFailure,
                                                   testAgainstSchema, withDb)
import           Network.HTTP.Client              (Manager)
import           System.IO                        (IO)
import           Test.Tasty                       (TestTree)

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests = makeTests "Tests of the doc interface"
          [ docSize
          , docGet
          , docPut
          , docDelete
          , docCopy
          ]

testDoc :: Value
testDoc = object [("_id", "foo"), ("llamas", Bool True)]

-- Doc-oriented functions
docSize :: IO Context -> TestTree
docSize =
  makeTests "Get document size and revision"
    [ withDb $ testAgainstFailure "No size information for non-existent doc" (Doc.size docGetDoc "foo" Nothing) NotFound
    , withDb $ testAgainstSchema
                 "Add a record and get all docs"
                 (\c -> do
                    _ :: Result Value <- Database.createDoc False testDoc c
                    Doc.size docGetDoc "foo" Nothing c)
                 "head--db-docid.json"
    ]

docGet :: IO Context -> TestTree
docGet =
  makeTests "Get document size and revision"
    [ withDb $ testAgainstFailure "No information for non-existent doc" (Doc.get docGetDoc "foo" Nothing) NotFound
    , withDb $ testAgainstSchema
                 "Add a doc and get the docs"
                 (\c -> do
                    _ :: Result Value <- Database.createDoc False testDoc c
                    Doc.get docGetDoc "foo" Nothing c)
                 "get--db-docid.json"
    ]

docPut :: IO Context -> TestTree
docPut =
  makeTests "Create and update a document"
    [ withDb $ testAgainstSchema "Simple add of document" (Doc.put docPutParam "foo" Nothing testDoc) "put--db-docid.json"
    , withDb $ testAgainstFailure "Failure to re-add document" (\c -> do
                                                                   _ :: Result Value <- Doc.put docPutParam "foo" Nothing testDoc c
                                                                   Doc.put docPutParam "foo" Nothing testDoc c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then update a doc"
                 (\c -> do
                    res <- Doc.put docPutParam "foo" Nothing testDoc c
                    let (Right (rev, _)) = getKey "rev" res
                    Doc.put docPutParam "foo" rev testDoc c)
                 "put--db-docid.json"
    ]

docDelete :: IO Context -> TestTree
docDelete =
  makeTests "Create and update a document"
    [ withDb $ testAgainstFailure "Delete non-existent document" (Doc.delete docPutParam "foo" Nothing) NotFound
    , withDb $ testAgainstFailure "Delete document with conflict" (\c -> do
                                                                   _ :: Result Value <- Doc.put docPutParam "foo" Nothing testDoc c
                                                                   Doc.delete docPutParam "foo" Nothing c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then delete doc"
                 (\c -> do
                    res <- Doc.put docPutParam "foo" Nothing testDoc c
                    let (Right (rev, _)) = getKey "rev" res
                    Doc.delete docPutParam "foo" rev c)
                 "delete--db-docid.json"
    ]

docCopy :: IO Context -> TestTree
docCopy =
  makeTests "Copy a document"
    [ withDb $ testAgainstFailure "Copy a non-existent document" (Doc.copy docPutParam "foo" Nothing "bar") NotFound
    , withDb $ testAgainstFailure "Copy a document with conflict" (\c -> do
                                                                   _ :: Result Value <- Doc.put docPutParam "foo" Nothing testDoc c
                                                                   _ :: Result Value <- Doc.put docPutParam "bar" Nothing testDoc c
                                                                   Doc.copy docPutParam "foo" Nothing "bar" c) Conflict
    , withDb $ testAgainstFailure
                 "Copy a document with a non-existent revision"
                 (\c -> do
                    _ :: Result Value <- Doc.put docPutParam "foo" Nothing testDoc c
                    Doc.copy docPutParam "foo" (Just $ DocRev "1-000000000") "bar" c)
                 NotFound
    , withDb $ testAgainstSchema
                 "Copy a document"
                 (\c -> do
                    _ :: Result Value <- Doc.put docPutParam "foo" Nothing testDoc c
                    Doc.copy docPutParam "foo" Nothing "bar" c)
                 "copy--db-docid.json"
    ]
