{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative                  ((<$>))
import           Data.Function                        (($))
import qualified Functionality.Explicit.Configuration as Configuration (tests)
import qualified Functionality.Explicit.Database      as Database (tests)
import qualified Functionality.Explicit.Design        as Design (tests)
import qualified Functionality.Explicit.Doc           as Doc (tests)
import qualified Functionality.Explicit.Local         as Local (tests)
import qualified Functionality.Explicit.Server        as Server (tests)
import qualified Functionality.Internal               as Internal (tests)
import           Functionality.Util                   (runTests)
import           Network.HTTP.Client                  (Manager)
import           System.IO                            (IO)
import           Test.Tasty                           (TestTree, testGroup)

main :: IO ()
main = runTests tests

tests :: Manager -> TestTree
tests manager = testGroup "All Tests" $
                  ($ manager) <$> [Internal.tests, Server.tests, Configuration.tests, Database.tests, Doc.tests, Design.tests, Local.tests]
