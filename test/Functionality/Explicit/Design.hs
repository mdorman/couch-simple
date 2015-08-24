{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Design where

import           Data.Aeson                     (Value, object)
import           Data.Either                    (Either (Right))
import           Data.Function                  (($))
import           Data.HashMap.Strict            (fromList)
import           Data.Maybe                     (Maybe (Just, Nothing))
import qualified Database.Couch.Explicit.Design as Design (allDocs, copy,
                                                           delete, get, info,
                                                           put, size)
import qualified Database.Couch.Explicit.Doc    as Doc (put)
import           Database.Couch.Response        (getKey)
import           Database.Couch.Types           (Context, CouchError (..),
                                                 CouchResult, DesignDoc (..),
                                                 DocRev (..),
                                                 ViewSpec (ViewSpec), ctxDb,
                                                 docGetDoc, docPutParam,
                                                 viewParams)
import           Functionality.Util             (makeTests, runTests,
                                                 testAgainstFailure,
                                                 testAgainstSchema, withDb)
import           Network.HTTP.Client            (Manager)
import           System.IO                      (IO)
import           Test.Tasty                     (TestTree)

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests = makeTests "Tests of the design doc interface"
          [ ddocSize
          , ddocGet
          , ddocPut
          , ddocDelete
          , ddocCopy
          , ddocInfo
          , viewAllDocs
          ]

-- Doc-oriented functions
ddocSize :: IO Context -> TestTree
ddocSize =
  makeTests "Get design document size and revision"
    [ testAgainstFailure "No size information for non-existent doc" (Design.size docGetDoc "llamas" Nothing) NotFound
    , testAgainstSchema "Get standard _auth ddoc in _users"  (\c -> Design.size docGetDoc "_auth" Nothing c { ctxDb = Just "_users" })

                 "head--db-_design-ddoc.json"
    ]

ddocGet :: IO Context -> TestTree
ddocGet =
  makeTests "Get design document content"
    [ testAgainstSchema "Get standard _auth ddoc in _users"  (\c -> Design.get docGetDoc "_auth" Nothing c { ctxDb = Just "_users" })
                 "get--db-_design-ddoc.json"
    ]

ddocPut :: IO Context -> TestTree
ddocPut =
  makeTests "Create and update a design document"
    [ withDb $ testAgainstSchema "Simple add of document" (Design.put docPutParam "foo" Nothing initialDdoc) "put--db-_design-ddoc.json"
    , withDb $ testAgainstFailure "Failure to update document" (\c -> do
                                                                    _ :: CouchResult Value <- Design.put docPutParam "foo" Nothing initialDdoc c
                                                                    Design.put docPutParam "foo" Nothing initialDdoc c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then update a doc"
                 (\c -> do
                    res <- Design.put docPutParam "foo" Nothing initialDdoc c
                    let (Right (id, _)) = getKey "id" res
                    let (Right (rev, _)) = getKey "rev" res
                    Design.put docPutParam "foo" (Just rev) initialDdoc {ddocId = id, ddocRev = rev} c)
                 "put--db-_design-ddoc.json"
    ]
  where
    initialDdoc = DesignDoc "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

ddocDelete :: IO Context -> TestTree
ddocDelete =
  makeTests "Create and update a design document"
    [ withDb $ testAgainstFailure "Delete non-existent design document" (Design.delete docPutParam "foo" Nothing) NotFound
    , withDb $ testAgainstFailure "Delete design document with conflict" (\c -> do
                                                                   _ :: CouchResult Value <- Design.put docPutParam "foo" Nothing initialDdoc c
                                                                   Design.delete docPutParam "foo" Nothing c) Conflict
    , withDb $ testAgainstSchema
                 "Add, then delete design doc"
                 (\c -> do
                    res <- Design.put docPutParam "foo" Nothing initialDdoc c
                    let (Right (rev, _)) = getKey "rev" res
                    Design.delete docPutParam "foo" rev c)
                 "delete--db-_design-ddoc.json"
    ]
  where
    initialDdoc = DesignDoc "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

ddocCopy :: IO Context -> TestTree
ddocCopy =
  makeTests "Copy a document"
    [ withDb $ testAgainstFailure "Copy a non-existent document" (Design.copy docPutParam "foo" Nothing "bar") NotFound
    , withDb $ testAgainstFailure "Copy a document with conflict" (\c -> do
                                                                   _ :: CouchResult Value <- Design.put docPutParam "foo" Nothing initialDdoc c
                                                                   _ :: CouchResult Value <- Design.put docPutParam "bar" Nothing initialDdoc c
                                                                   Design.copy docPutParam "foo" Nothing "bar" c) Conflict
    , withDb $ testAgainstFailure
                 "Copy a document with a non-existent revision"
                 (\c -> do
                    _ :: CouchResult Value <- Design.put docPutParam "foo" Nothing initialDdoc c
                    Design.copy docPutParam "foo" (Just $ DocRev "1-000000000") "bar" c)
                 NotFound
    , withDb $ testAgainstSchema
                 "Copy a document"
                 (\c -> do
                    _ :: CouchResult Value <- Design.put docPutParam "foo" Nothing initialDdoc c
                    Design.copy docPutParam "foo" Nothing "bar" c)
                 "copy--db-_design-ddoc.json"
    ]
  where
    initialDdoc = DesignDoc "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

ddocInfo :: IO Context -> TestTree
ddocInfo =
  makeTests "Get design document info"
    [ testAgainstSchema "Get standard _auth ddoc in _users"  (\c -> Design.info "_auth" c { ctxDb = Just "_users" })
                 "get--db-_design-ddoc-_info.json"
    ]

viewAllDocs :: IO Context -> TestTree
viewAllDocs =
  makeTests "Get results from a view" [
    withDb $ testAgainstSchema "Simple view" checkView "get--db-_design-ddoc-_view-view.json"
    ]
  where
    checkView c = do
      _ :: CouchResult Value <- Design.put docPutParam "foo" Nothing initialDdoc c
      _ :: CouchResult Value <- Doc.put docPutParam "foo" Nothing testDoc c
      Design.allDocs viewParams "foo" "test" c
    initialDdoc = DesignDoc "" "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just $ fromList [("test", simpleView)])
    simpleView = ViewSpec "function(doc) {\n  if(doc.date && doc.title) {\n    emit(doc.date, doc.title);\n  }\n}\n" Nothing
    testDoc = object [("_id", "the-silence-of-the-lambs"), ("title", "The Silence of the Lambs"), ("date", "1991-02-14")]
