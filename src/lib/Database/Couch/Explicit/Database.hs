{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

{- |

Module      : Database.Couch.Explicit.Database
Description : Database-oriented requests to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has
been made to keep names of types or functions from clashing with
obvious or otherwise commonly-used names.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/database/index.html.

-}

module Database.Couch.Explicit.Database where

import Control.Applicative (
  (<$>),
  (<*>),
  )
import Control.Monad (
  (>>=),
  return,
  when,
  )
import Control.Monad.IO.Class (
  MonadIO,
  )
import Data.Aeson (
  ToJSON,
  Value (Object),
  toJSON,
  )
import Data.Bool (
  Bool (False, True)
  )
import Data.Either (
  Either,
  )
import Data.Function (
  ($),
  (.),
  )
import Data.HashMap.Strict (
  fromList,
  )
import Data.Maybe (
  Maybe (Just),
  catMaybes,
  fromJust,
  isJust,
  )
import Database.Couch.Internal (
  makeJsonRequest,
  )
import Database.Couch.RequestBuilder (
  addPath,
  selectDb,
  setHeaders,
  setJsonBody,
  setMethod,
  setQueryParam,
  )
import Database.Couch.ResponseParser (
  checkStatusCode,
  failed,
  getKey,
  responseStatus,
  responseValue,
  toOutputType,
  )
import Database.Couch.Types (
  Context,
  CouchError (Unknown),
  CreateResult (WithRev, NoRev),
  DbAllDocs,
  DbBulkDocs,
  DocId,
  allOrNothing,
  fullCommit,
  newEdits,
  toQueryParameters,
  )
import Network.HTTP.Client (
  CookieJar,
  )
import Network.HTTP.Types (
  statusCode,
  )

-- | Check that the requested database exists.
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/common.html#head--db API documentation>
--
-- Returns 'False' or 'True' as appropriate.
--
-- Status: __Complete__
exists :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
exists =
  makeJsonRequest request parse
  where
    request = do
      selectDb
      setMethod "HEAD"
    parse = do
      -- Check status codes by hand because we don't want 404 to be an
      -- error, just False
      s <- responseStatus
      case statusCode s of
       200 -> return True
       404 -> return False
       _   -> failed Unknown

-- | Get most basic meta-information.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--db API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'Value'.
--
-- Status: __Complete__
meta :: MonadIO m => Context -> m (Either CouchError (Value, Maybe CookieJar))
meta =
  makeJsonRequest request parse
  where
    request =
      selectDb
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Create a database
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#put--db API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'Value'.
--
-- Status: __Complete__
create :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
create =
  makeJsonRequest request parse
  where
    request = do
      selectDb
      setMethod "PUT"
    parse = do
      checkStatusCode
      return True

-- | delete a database
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#delete--db API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'Value'.
--
-- Status: __Complete__
delete :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
delete =
  makeJsonRequest request parse
  where
    request = do
      selectDb
      setMethod "DELETE"
    parse = do
      checkStatusCode
      return True

-- | create a new document in a database
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#post--db API documentation>
--
-- The constructor for the return type depends on whether the create
-- was done in batch mode, as there is no avenue for returning a
-- 'DocRev' in that circumstance.
--
-- Status: __Complete__
createDoc :: (MonadIO m, ToJSON a) => Bool -> a -> Context -> m (Either CouchError (CreateResult, Maybe CookieJar))
createDoc batch doc =
  makeJsonRequest request parse
  where
    request = do
      selectDb
      setMethod "POST"
      when batch
        (setQueryParam [("batch", Just "ok")])
      setJsonBody doc
    parse = do
      checkStatusCode
      if batch
        then NoRev <$> (getKey "id" >>= toOutputType)
        else WithRev <$> (getKey "id" >>= toOutputType) <*> (getKey "rev" >>= toOutputType)

-- | Get a list of all database documents.
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/bulk-api.html#get--db-_all_docs API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- a 'Value' for you to take apart.
--
-- Status: __Complete__
allDocs :: MonadIO m => DbAllDocs -> Context -> m (Either CouchError (Value, Maybe CookieJar))
allDocs param =
  makeJsonRequest request parse
  where
    request = do
      selectDb
      addPath "_all_docs"
      setQueryParam $ toQueryParameters param
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Get a list of some database documents.
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/bulk-api.html#post--db-_all_docs API documentation>
--
-- There's some ambiguity in the documentation as to whether this
-- accepts any query parameters.
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'List' of 'Value'.
--
-- Status: __Limited?__
someDocs :: MonadIO m => [DocId] -> Context -> m (Either CouchError (Value, Maybe CookieJar))
someDocs ids =
  makeJsonRequest request parse
  where
    request = do
      setMethod "POST"
      selectDb
      addPath "_all_docs"
      let parameters = Object (fromList [("keys",toJSON ids)])
      setJsonBody parameters
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Create or update a list of documents.
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/bulk-api.html#post--db-_bulk_docs API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'List' of 'Value'.
--
-- Status: __Complete__
bulkDocs :: (MonadIO m, ToJSON a) => DbBulkDocs -> [a] -> Context -> m (Either CouchError ([Value], Maybe CookieJar))
bulkDocs param docs =
  makeJsonRequest request parse
  where
    request = do
      setMethod "POST"
      when (isJust $ fullCommit param)
        (setHeaders [("X-Couch-Full-Commit", if fromJust $ fullCommit param then "true" else "false")])
      selectDb
      addPath "_bulk_docs"
      let parameters = Object ((fromList . catMaybes) [Just ("docs",toJSON docs), boolToParam "all_or_nothing" allOrNothing, boolToParam "new_edits" newEdits])
      setJsonBody parameters
    parse = do
      checkStatusCode
      responseValue >>= toOutputType
    boolToParam k s = do
      v <- s param
      return (k, if v then "true" else "false")
