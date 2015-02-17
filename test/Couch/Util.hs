{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Couch.Util where

import Control.Lens (
  _1,
  _2,
  _Left,
  _Nothing,
  _Right,
  has,
  preview,
  )
import Data.Aeson (
  Value,
  )
import Data.Aeson.Lens (
  _Object,
  )
import Data.Bool (
  Bool,
  )
import Data.Either (
  Either,
  )
import Data.Function (
  ($),
  (.),
  )
import Data.Maybe (
  Maybe (Just),
  )
import Database.Couch.Types (
  CouchError (..),
  )
import Network.HTTP.Client (
  CookieJar,
  )
import Test.Tasty.HUnit (
  Assertion,
  assertBool,
  assertEqual,
  )

checkBooleanResult :: Bool -> Either CouchError (Bool, Maybe CookieJar) -> Assertion
checkBooleanResult b res = assertEqual "should have a true value" (Just b) $ preview (_Right._1) res

checkEmptyCookieJar :: Either CouchError (a, Maybe CookieJar) -> Assertion
checkEmptyCookieJar res = assertBool "should have an empty cookie jar" (has (_Right._2._Nothing) res)

checkObject :: Either CouchError (Value, Maybe CookieJar) -> Assertion
checkObject res = assertBool "should have an object" $ has (_Right._1._Object) res

checkRequestFailure :: Either CouchError a -> Assertion
checkRequestFailure res = assertBool "should have failed" $ has _Left res

checkRequestSuccess :: Either CouchError a -> Assertion
checkRequestSuccess res = assertBool "should have succeeded" $ has _Right res

basicSuccess :: Either CouchError (a, Maybe CookieJar) -> Assertion
basicSuccess res = do
  checkEmptyCookieJar res
  checkRequestSuccess res
