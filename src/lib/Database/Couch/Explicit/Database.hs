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
  Bool (False, True)
  )
import Data.Either (
  Either,
  )
import Data.Maybe (
  Maybe,
  )
import Database.Couch.Internal (
  makeJsonRequest,
  )
import Database.Couch.RequestBuilder (
  selectDb,
  setMethod,
  )
import Database.Couch.ResponseParser (
  checkStatusCode,
  failed,
  responseStatus,
  responseValue,
  toOutputType,
  )
import Database.Couch.Types (
  Context,
  CouchError (Unknown),
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
