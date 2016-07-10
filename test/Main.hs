{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad             (liftM, return, (=<<))
import           Control.Monad.Catch       (SomeException, handle)
import           Data.Bool                 (Bool (False))
import           Data.Function             (($), (.))
import           Data.Monoid               ((<>))
import qualified Functionality             (tests)
import           Network.HTTP.Client       (defaultManagerSettings, httpNoBody,
                                            newManager, parseRequest_,
                                            responseStatus)
import           Network.HTTP.Types.Status (statusIsSuccessful)
import qualified Quality                   (tests)
import           System.IO                 (IO)
import           Test.Tasty                (TestTree, defaultMain, testGroup)

-- Run all teests by default
main :: IO ()
main = defaultMain . tests =<< do
  manager <- newManager defaultManagerSettings
  let request = parseRequest_ "http://localhost:5984/"
  handle (\(_ :: SomeException) -> return False) $ liftM (statusIsSuccessful . responseStatus) (httpNoBody request manager)

-- Everything we can throw at it
tests :: Bool -> TestTree
tests haveCouch = testGroup "All tests" $
  [Quality.tests] <> [Functionality.tests | haveCouch]
