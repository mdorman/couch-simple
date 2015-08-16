{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Design where

import           Data.Maybe                     (Maybe (Just, Nothing))
import qualified Database.Couch.Explicit.Design as Design (get, size)
import           Database.Couch.Types           (Context, CouchError (..),
                                                 ctxDb, docGetDoc)
import           Functionality.Util             (makeTests, runTests,
                                                 testAgainstFailure,
                                                 testAgainstSchema)
import           Network.HTTP.Client            (Manager)
import           System.IO                      (IO)
import           Test.Tasty                     (TestTree)

_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests = makeTests "Tests of the design doc interface"
          [ ddocSize
          , ddocGet
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
