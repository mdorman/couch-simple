{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality where

import           Control.Applicative                  ((<$>))
import           Control.Monad                        (return)
import           Data.Function                        (const, ($))
import qualified Functionality.Explicit.Configuration as Configuration (tests)
import qualified Functionality.Explicit.Database      as Database (tests)
import qualified Functionality.Explicit.Design        as Design (tests)
import qualified Functionality.Explicit.Doc           as Doc (tests)
import qualified Functionality.Explicit.Local         as Local (tests)
import qualified Functionality.Explicit.Server        as Server (tests)
import qualified Functionality.Internal               as Internal (tests)
import           Network.HTTP.Client                  (defaultManagerSettings,
                                                       newManager)
import           System.IO                            (IO)
import           Test.Tasty                           (TestTree, defaultMain,
                                                       testGroup, withResource)

-- For interactive testing
_main :: IO ()
_main = defaultMain tests

-- Tests for functionality
tests :: TestTree
tests = withResource
        (newManager defaultManagerSettings)
        (const $ return ()) $ \manager -> testGroup "Functional Tests" $
                                          ($ manager) <$> [Internal.tests, Server.tests, Configuration.tests, Database.tests, Doc.tests, Design.tests, Local.tests]
