{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Local where

import           Data.Aeson                    (Value (Bool), object)
import           Data.Bool                     (Bool (True))
import           Data.Either                   (Either (Right))
import           Data.Function                 (($))
import           Data.Maybe                    (Maybe (Just, Nothing))
import qualified Database.Couch.Explicit.Local as Local (copy, delete, get, put)
import           Database.Couch.Response       (getKey)
import           Database.Couch.Types          (Context, DocRev (DocRev),
                                                Error (Conflict, NotFound),
                                                Result, modifyDoc, retrieveDoc)
import           Functionality.Util            (makeTests, runTests,
                                                testAgainstFailure,
                                                testAgainstSchema, withDb)
import           Network.HTTP.Client           (Manager)
import           System.IO                     (IO)
import           Test.Tasty                    (TestTree)

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests = makeTests "Tests of the local interface"
          [ docGet
          , docPut
          , docDelete
          , docCopy
          ]

testDoc :: Value
testDoc = object [("_id", "foo"), ("llamas", Bool True)]

docGet :: IO Context -> TestTree
docGet =
  makeTests "Get local document size and revision"
    [ withDb $ testAgainstFailure "No information for non-existent doc" (Local.get retrieveDoc "foo" Nothing) NotFound
    , withDb $ testAgainstSchema
                 "Add a doc and get the docs"
                 (\c -> do
                    _ :: Result Value <- Local.put modifyDoc "foo" Nothing testDoc c
                    Local.get retrieveDoc "foo" Nothing c)
                 "get--db-docid.json"
    ]

docPut :: IO Context -> TestTree
docPut =
  makeTests "Create and update a local document"
    [ withDb $ testAgainstSchema "Simple add of local document" (Local.put modifyDoc "foo" Nothing testDoc) "put--db-docid.json"
    , withDb $ testAgainstFailure "Failure to re-add local document" (\c -> do
                                                                   _ :: Result Value <- Local.put modifyDoc "foo" Nothing testDoc c
                                                                   Local.put modifyDoc "foo" Nothing testDoc c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then update a doc"
                 (\c -> do
                    res <- Local.put modifyDoc "foo" Nothing testDoc c
                    let (Right (rev, _)) = getKey "rev" res
                    Local.put modifyDoc "foo" rev testDoc c)
                 "put--db-docid.json"
    ]

docDelete :: IO Context -> TestTree
docDelete =
  makeTests "Delete a local document"
    [ withDb $ testAgainstFailure "Delete non-existent local document" (Local.delete modifyDoc "foo" Nothing) NotFound
    , withDb $ testAgainstFailure "Delete local document with conflict" (\c -> do
                                                                   _ :: Result Value <- Local.put modifyDoc "foo" Nothing testDoc c
                                                                   Local.delete modifyDoc "foo" Nothing c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then delete doc"
                 (\c -> do
                    res <- Local.put modifyDoc "foo" Nothing testDoc c
                    let (Right (rev, _)) = getKey "rev" res
                    Local.delete modifyDoc "foo" rev c)
                 "delete--db-docid.json"
    ]

docCopy :: IO Context -> TestTree
docCopy =
  makeTests "Copy a document"
    [ withDb $ testAgainstFailure "Copy a non-existent document" (Local.copy modifyDoc "foo" Nothing "bar") NotFound
    , withDb $ testAgainstFailure "Copy a document with conflict" (\c -> do
                                                                   _ :: Result Value <- Local.put modifyDoc "foo" Nothing testDoc c
                                                                   _ :: Result Value <- Local.put modifyDoc "bar" Nothing testDoc c
                                                                   Local.copy modifyDoc "foo" Nothing "bar" c) Conflict
    , withDb $ testAgainstFailure
                 "Copy a document with a non-existent revision"
                 (\c -> do
                    _ :: Result Value <- Local.put modifyDoc "foo" Nothing testDoc c
                    Local.copy modifyDoc "foo" (Just $ DocRev "1-000000000") "bar" c)
                 NotFound
    , withDb $ testAgainstSchema
                 "Copy a document"
                 (\c -> do
                    _ :: Result Value <- Local.put modifyDoc "foo" Nothing testDoc c
                    Local.copy modifyDoc "foo" Nothing "bar" c)
                 "copy--db-docid.json"
    ]
