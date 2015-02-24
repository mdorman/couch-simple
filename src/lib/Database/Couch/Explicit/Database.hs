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
import Data.Text.Encoding (
  encodeUtf8,
  )
import Database.Couch.Internal (
  makeJsonRequest,
  )
import Database.Couch.RequestBuilder (
  RequestBuilder,
  addPath,
  selectDb,
  selectDoc,
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
  DbChanges,
  DocId,
  bdAllOrNothing,
  bdFullCommit,
  bdNewEdits,
  cLastEvent,
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
      when (isJust $ bdFullCommit param)
        (setHeaders [("X-Couch-Full-Commit", if fromJust $ bdFullCommit param then "true" else "false")])
      selectDb
      addPath "_bulk_docs"
      let parameters = Object ((fromList . catMaybes) [Just ("docs",toJSON docs), boolToParam "all_or_nothing" bdAllOrNothing, boolToParam "new_edits" bdNewEdits])
      setJsonBody parameters
    parse = do
      checkStatusCode
      responseValue >>= toOutputType
    boolToParam k s = do
      v <- s param
      return (k, if v then "true" else "false")

-- | Get a list of all document modifications .
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/changes.html#get--db-_changes API documentation>
--
-- This call does not stream out results; therefore, it also doesn't
-- allow any specification of parameters for streaming.
--
-- The returned data is variable enough we content ourselves with just
-- returning a 'Value'.
--
-- Status: __Limited__
changes :: MonadIO m => DbChanges -> Context -> m (Either CouchError (Value, Maybe CookieJar))
changes param =
  makeJsonRequest request parse
  where
    request = do
      when (isJust $ cLastEvent param)
        (setHeaders [("Last-Event-Id", encodeUtf8 . fromJust $ cLastEvent param)])
      selectDb
      addPath "_changes"
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Encode the common bits for our two compact calls
compactBase :: RequestBuilder ()
compactBase = do
      setMethod "POST"
      selectDb
      addPath "_compact"

-- | Compact a database
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_compact API documentation>
--
-- Run the compaction process on an entire database
--
-- Status: __Complete__
compact :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
compact =
  makeJsonRequest request parse
  where
    request =
      compactBase
    parse = do
      checkStatusCode
      getKey "ok" >>= toOutputType

-- | Compact the views attached to a particular design document
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_compact-ddoc API documentation>
--
-- Run the compaction process on the views associated with the specified design document
--
-- Status: __Complete__
compactDesignDoc :: MonadIO m => DocId -> Context -> m (Either CouchError (Bool, Maybe CookieJar))
compactDesignDoc doc =
  makeJsonRequest request parse
  where
    request = do
      compactBase
      selectDoc doc
    parse = do
      checkStatusCode
      getKey "ok" >>= toOutputType

-- | Ensure that all changes to the database have made it to disk
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_ensure_full_commit API documentation>
--
-- The start time for the instance doesn't seem very interesting,
-- especially for this particular operation, so I haven't bothered to
-- try and return it.
--
-- Status: __Complete__
sync :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
sync =
  makeJsonRequest request parse
  where
    request = do
      setMethod "POST"
      selectDb
      addPath "_ensure_full_commit"
    parse = do
      checkStatusCode
      getKey "ok" >>= toOutputType

-- | Cleanup any stray view definitions
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_view_cleanup API documentation>
--
-- Clean up out of data view indices, which follow from changes to the
-- view content.
--
-- Status: __Complete__
cleanup :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
cleanup =
  makeJsonRequest request parse
  where
    request = do
      setMethod "POST"
      selectDb
      addPath "_view_cleanup"
    parse = do
      checkStatusCode
      getKey "ok" >>= toOutputType

-- | Get security information for database
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/security.html#get--db-_security API documentation>
--
-- Although there are base requirements for the content this returns
-- ("admin" and "members" keys, which each contain "users" and
-- "roles"), the system does not prevent you from adding (and even
-- using in validation functions) additional fields, so we keep the
-- return value general
--
-- Status: __Complete__
getSecurity :: MonadIO m => Context -> m (Either CouchError (Value, Maybe CookieJar))
getSecurity =
  makeJsonRequest request parse
  where
    request = do
      setMethod "GET"
      selectDb
      addPath "_security"
    parse = do
      checkStatusCode
      responseValue >>= toOutputType

-- | Set security information for database
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/security.html#post--db-_security API documentation>
--
-- Although there are base requirements for the content this returns
-- ("admin" and "members" keys, which each contain "users" and
-- "roles"), the system does not prevent you from adding (and even
-- using in validation functions) additional fields, so we keep the
-- return value general
--
-- Status: __Complete__
setSecurity :: MonadIO m => Context -> m (Either CouchError (Bool, Maybe CookieJar))
setSecurity =
  makeJsonRequest request parse
  where
    request = do
      setMethod "POST"
      selectDb
      addPath "_security"
    parse = do
      checkStatusCode
      getKey "ok" >>= toOutputType
