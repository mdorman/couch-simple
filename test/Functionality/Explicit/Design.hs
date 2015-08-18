{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Design where

import           Control.Monad                  (return)
import           Data.Aeson                     (Value, object)
import           Data.Either                    (Either (Right))
import           Data.Function                  (($))
import           Data.Maybe                     (Maybe (Just, Nothing))
import           Data.Text.IO                   (putStrLn)
import qualified Database.Couch.Explicit.Design as Design (get, put, size)
import           Database.Couch.Response        (getKey)
import           Database.Couch.Types           (Context, CouchError (..),
                                                 CouchResult, DesignDoc (..),
                                                 ctxDb, docGetDoc, docPutParam)
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
