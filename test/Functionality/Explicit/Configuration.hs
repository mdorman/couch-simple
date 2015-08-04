{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Explicit.Configuration where

import           Control.Applicative                   ((<$>))
import           Data.Function                         (($))
import qualified Database.Couch.Explicit.Configuration as Configuration (section,
                                                                         server)
import           Database.Couch.Types                  (Context)
import           Functionality.Util                    (runTests, serverContext,
                                                        testAgainstSchema)
import           Network.HTTP.Client                   (Manager)
import           System.IO                             (IO)
import           Test.Tasty                            (TestTree, testGroup)

_main :: IO ()
_main = runTests tests

-- We specifically don't use makeTests here because we want no-databas-selected context
tests :: Manager -> TestTree
tests manager =
  testGroup "Tests of the config interface" $
  ($ serverContext manager) <$> [server, section]

-- Server-oriented functions
server :: IO Context -> TestTree
server = testAgainstSchema "Get server config" Configuration.server "get--_config.json"

section :: IO Context -> TestTree
section = testAgainstSchema "Get section config" (Configuration.section "couchdb") "get--_config-section.json"
