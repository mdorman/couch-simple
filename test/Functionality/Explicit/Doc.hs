{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Doc where

import           Control.Monad                    (void)
import           Data.Aeson                       (Value (Bool), object)
import           Data.Bool                        (Bool (False, True))
import           Data.Function                    (($))
import           Data.Maybe                       (Maybe (Nothing))
import qualified Database.Couch.Explicit.Doc      as Doc (size)
import qualified Database.Couch.Explicit.Database as Database (createDoc)
import           Database.Couch.Types             (Context, CouchError (..),
                                                   docGetDoc)
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
          ]

-- Doc-oriented functions
docSize :: IO Context -> TestTree
docSize =
  makeTests "Get document size and revision"
    [ withDb $ testAgainstFailure "No size information for non-existent doc" (Doc.size docGetDoc "foo" Nothing) NotFound
    , withDb $ testAgainstSchema
                 "Add a record and get all docs"
                 (\c -> do
                    void $ Database.createDoc False (object [("_id", "foo"), ("llamas", Bool True)]) c
                    Doc.size docGetDoc "foo" Nothing c)
                 "head--db-docid.json"
    ]
