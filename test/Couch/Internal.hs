{-# LANGUAGE OverloadedStrings #-}

module Couch.Internal (
  getInternalTests,
) where

import Control.Error.Util (
  isRight
  )
import Data.Aeson (
  Value (Object),
  )
import Data.Default (
  def
  )
import Data.HashMap.Strict (
  lookupDefault,
  )
import Database.Couch.Internal (
  jsonRequest,
  )
import Network.HTTP.Client (
  RequestBody (RequestBodyLBS),
  checkStatus,
  closeManager,
  defaultManagerSettings,
  host,
  method,
  newManager,
  path,
  port,
  requestBody,
  requestHeaders,
  )
import Test.Hspec (
  it,
  runIO,
  shouldBe,
  )
import Test.Tasty (
  TestTree,
  testGroup,
  withResource,
  )
import Test.Tasty.Hspec (
  testCase
  )

getInternalTests :: IO TestTree
getInternalTests =
  return (withResource (newManager defaultManagerSettings) (closeManager) allTests)
  where
    allTests getManager = do
      testGroup "jsonRequest tests" [
        testCase "Retrieve simple document" $ do
           let req = def { checkStatus = const . const . const Nothing, requestHeaders = [], host = "localhost", method = "GET", path = "/", port = 5984, requestBody = RequestBodyLBS "" }
           res <- runIO $ do
                  manager <- getManager
                  jsonRequest manager req
           let success = isRight res
               value =
                 case res of
                   Right (_, _, Object o) -> Right $ lookupDefault "Key not present" "couchdb" o
                   Right (_, _, val) -> Left $ show val
                   Left err -> Left $ show err
           it "should have succeeded" $
             success `shouldBe` True
           it "should have a couchdb welcome message" $
             value `shouldBe` Right "Welcome"
        ]
