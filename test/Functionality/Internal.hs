{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Internal where

import           Data.Aeson.Parser       (json)
import           Data.Default            (def)
import           Data.Either             (Either (Left, Right))
import           Data.Eq                 ((==))
import           Data.Foldable           (find)
import           Data.Function           (($), (.))
import           Data.Functor            (fmap)
import           Data.Maybe              (Maybe (Just))
import           Data.Tuple              (fst, snd)
import           Database.Couch.Internal (parsedRequest)
import           Functionality.Util      (checkSchema, runTests)
import           Network.HTTP.Client     (Manager, RequestBody (RequestBodyLBS),
                                          host, method, path, port, requestBody,
                                          requestHeaders)
import           Network.HTTP.Types      (status200)
import           System.IO               (IO)
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (assertFailure, testCaseSteps, (@=?))
import           Text.Show               (show)

-- For interactive testing
_main :: IO ()
_main = runTests tests

tests :: Manager -> TestTree
tests manager = testGroup "Raw JSON interface" [requestRoot manager]

-- The root of the couchdb server provides predictable content
requestRoot :: Manager -> TestTree
requestRoot manager = testCaseSteps "Check parsedRequest" $ \step -> do
  step "Request root"
  res <- parsedRequest json manager def { requestHeaders = [], host = "localhost", method = "GET", path = "/", port = 5984, requestBody = RequestBodyLBS "" }
  step "No exception"
  case res of
    Left error -> assertFailure (show error)
    Right (headers, status, cookieJar, value) -> do
      step "Cache-Control header"
      Just "must-revalidate" @=? fmap snd (find ((== "cache-control") . fst) headers)
      step "200 status code"
      status200 @=? status
      step "Empty cookie jar"
      def @=? cookieJar
      checkSchema step value "get--.json"
