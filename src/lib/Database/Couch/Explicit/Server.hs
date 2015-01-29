{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{- |

Module      : Database.Couch.Explicit.Server
Description : Server-oriented requests to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has
been made to keep names of types or functions from clashing with
obvious or otherwise commonly-used names.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/server/index.html.

-}

module Database.Couch.Explicit.Server where

import Control.Monad (
  (>>=),
  return,
  )
import Control.Monad.IO.Class (
  MonadIO,
  )
import Data.Aeson (
  Value,
  )
import Data.Bool (
  Bool
  )
import Data.Either (
  Either,
  )
import Data.Function (
  ($),
  (.),
  )
import Data.Int (
  Int,
  )
import Data.List (
  map,
  )
import Data.Maybe (
  Maybe (Just),
  catMaybes,
  )
import Data.String (
  fromString,
  )
import Data.Text (
  Text,
  intercalate,
  splitAt,
  )
import Data.Text.Encoding (
  encodeUtf8,
  )
import Data.UUID (
  UUID,
  fromASCIIBytes,
  )
import Database.Couch.Internal (
  makeJsonRequest,
  )
import Database.Couch.RequestBuilder (
  setMethod,
  addPath,
  addQueryParam,
  )
import Database.Couch.ResponseParser (
  checkStatusCode,
  getKey,
  responseValue,
  toOutputType,
  )
import Database.Couch.Types (
  Context,
  CouchError,
  )
import GHC.Err (
  undefined,
  )
import Network.HTTP.Client (
  CookieJar,
  )
import Text.Show (
  show
  )

-- | Get most basic meta-information.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'Value'.
--
-- Status: __Complete__
meta :: MonadIO m => Context -> m (Either CouchError (Value, Maybe CookieJar))
meta =
  makeJsonRequest request parse
  where
    -- This is actually the default request
    request =
      return ()
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Get a list of active tasks.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#active-tasks API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'List' of 'Value'.
--
-- Status: __Complete__
activeTasks :: MonadIO m => Context -> m (Either CouchError ([Value], Maybe CookieJar))
activeTasks =
  makeJsonRequest request parse
  where
    request = do
      addPath "_active_tasks"
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Get a list of all databases.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#all-dbs API documentation>
--
-- The returned data is decoded into a 'List' of 'Text'.
--
-- Status: __Complete__
allDbs :: MonadIO m => Context -> m (Either CouchError ([Text], Maybe CookieJar))
allDbs =
  makeJsonRequest request parse
  where
    request = do
      addPath "_all_dbs"
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Get a list of all database events.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#db-updates API documentation>
--
-- This call does not stream out results; therefore, it also doesn't
-- allow any specification of parameters for streaming.
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'List' of 'Value'.
--
-- Status: __Limited__
dbUpdates :: MonadIO m => Context -> m (Either CouchError ([Value], Maybe CookieJar))
dbUpdates =
  makeJsonRequest request parse
  where
    request = do
      addPath "_db_updates"
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Get the log output of the server
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#log API documentation>
--
-- This call doesn't return a JSON result, so we're deferring support
-- for the moment.
--
-- Status: __Unimplemented__
log :: MonadIO m => Context -> m (Either CouchError (Text, Maybe CookieJar))
log = undefined

-- | Administer replication for databases on the server
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#replicate API documentation>
--
-- FIXME: We need to define arguments that get translated into the
-- appropriate query parameters, as well as combinators, localDb and
-- remoteDb that will produce appropriate Url parameters.
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'Value'.
--
-- Status: __Broken__
replicate :: MonadIO m => Context -> m (Either CouchError (Value, Maybe CookieJar))
replicate =
  makeJsonRequest request parse
  where
    request = do
      setMethod "POST"
      addPath "_replicate"
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Restart the server
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#restart API documentation>
--
-- The response should indicate whether it accepted the request,
-- though the actual restart will happen at some arbitrary later
-- point.
--
-- Status: __Complete__
restart :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
restart =
  makeJsonRequest request parse
  where
    request = do
      setMethod "POST"
      addPath "_restart"
    parse = do
      checkStatusCode
      getKey "ok" >>= toOutputType

-- | Get server statistics
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#stats API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'Value'.
--
-- Status: __Complete__
stats :: MonadIO m => Context -> m (Either CouchError (Value, Maybe CookieJar))
stats =
  makeJsonRequest request parse
  where
    request = do
      addPath "_stats"
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Get a batch of UUIDs
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#uuids API documentation>
--
-- Turns the string output into actual 'UUID's.
--
-- Status: __Complete__
uuids :: MonadIO m => Int -> Context -> m (Either CouchError ([UUID], Maybe CookieJar))
uuids count =
  makeJsonRequest request parse
  where
    request = do
      addPath "_uuids"
      addQueryParam [("count", Just $ fromString $ show count)]
    parse = do
      checkStatusCode
      val <- getKey "uuids" >>= toOutputType
      return $ catMaybes $ map (fromASCIIBytes . encodeUtf8 . reformatUuid) val
    reformatUuid s =
      let (first, second') = splitAt 8 s
          (second, third') = splitAt 4 second'
          (third, fourth') = splitAt 4 third'
          (fourth, fifth) = splitAt 4 fourth'
      in intercalate "-" [first, second, third, fourth, fifth]
