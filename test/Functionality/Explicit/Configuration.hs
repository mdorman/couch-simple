{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Functionality.Explicit.Configuration where

import           Control.Applicative                   ((<$>))
import           Data.Aeson                            (Value (String))
import           Data.Function                         (($))
import qualified Database.Couch.Explicit.Configuration as Configuration (delValue, getValue, section,
                                                                         server, setValue)
import           Database.Couch.Types                  (Context, Result)
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
  ($ serverContext manager) <$> [server, section, getValue, setValue, delValue]

-- Server-oriented functions
server :: IO Context -> TestTree
server = testAgainstSchema "Get server config" Configuration.server "get--_config.json"

section :: IO Context -> TestTree
section = testAgainstSchema "Get section config" (Configuration.section "couchdb") "get--_config-section.json"

getValue :: IO Context -> TestTree
getValue = testAgainstSchema "Get config item" (Configuration.getValue "couchdb" "max_document_size") "get--_config-section-key.json"

setValue :: IO Context -> TestTree
setValue = testAgainstSchema "Set config item" (Configuration.setValue "testsection" "testkey" $ String "foo") "put--_config-section-key.json"

delValue :: IO Context -> TestTree
delValue = testAgainstSchema "Delete config item" (\c -> do
                                                       _ :: Result Value <- Configuration.setValue "testsection" "testkey" (String "foo") c
                                                       Configuration.delValue "testsection" "testkey" c) "delete--_config-section-key.json"
