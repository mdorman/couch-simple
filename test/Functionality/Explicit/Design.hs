{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Design where

import           Data.Maybe                     (Maybe (Just, Nothing))
import qualified Database.Couch.Explicit.Design as Design (size)
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
          ]

-- Doc-oriented functions
ddocSize :: IO Context -> TestTree
ddocSize =
  makeTests "Get design document size and revision"
    [ testAgainstFailure "No size information for non-existent doc" (Design.size docGetDoc "llamas" Nothing) NotFound
    , testAgainstSchema "Get standard _auth ddoc in _users"  (\c -> Design.size docGetDoc "_auth" Nothing c { ctxDb = Just "_users" })

                 "head--db-_design-ddoc.json"
    ]
