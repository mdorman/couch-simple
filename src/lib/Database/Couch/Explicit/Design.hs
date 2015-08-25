{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Explicit.Design
Description : Design Document-oriented requests to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has
been made to keep names of types or functions from clashing with
obvious or otherwise commonly-used names.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/doc/index.html.

-}

module Database.Couch.Explicit.Design where

import           Control.Monad.IO.Class          (MonadIO)
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  Value (Null, Number), object,
                                                  toJSON)
import           Data.Function                   (($))
import           Data.Maybe                      (Maybe (Nothing))
import qualified Database.Couch.Explicit.DocBase as Base (accessBase, copy,
                                                          delete, get, put)
import           Database.Couch.Internal         (standardRequest,
                                                  structureRequest)
import           Database.Couch.RequestBuilder   (RequestBuilder, addPath,
                                                  selectDoc, setJsonBody,
                                                  setMethod, setQueryParam)
import           Database.Couch.ResponseParser   (checkStatusCode, failed,
                                                  getContentLength, getDocRev,
                                                  responseStatus, toOutputType)
import           Database.Couch.Types            (Context, CouchResult,
                                                  DocGetDoc, DocId, DocPut,
                                                  DocRev, Error (Unknown),
                                                  ViewParams, toQueryParameters,
                                                  unwrapDocRev)
import           GHC.Num                         (fromInteger)
import           Network.HTTP.Types              (statusCode)

-- | Get the size and revision of the specified design document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#head--db-_design-ddoc API documentation>
--
-- If the specified DocRev matches, returns a JSON Null, otherwise a
-- JSON hash of [(Int, DocRev)].
--
-- Status: __Broken__
size :: (FromJSON a, MonadIO m) => DocGetDoc -> DocId -> Maybe DocRev -> Context -> m (CouchResult a)
size param doc rev =
  structureRequest request parse
  where
    request = do
      setMethod "HEAD"
      Base.accessBase "_design" doc rev
      setQueryParam $ toQueryParameters param
    parse = do
      -- Do our standard status code checks
      checkStatusCode
      -- And then handle 304 appropriately
      s <- responseStatus
      docRev <- getDocRev
      contentLength <- getContentLength
      case statusCode s of
        200 -> toOutputType $ object [(unwrapDocRev docRev, Number $ fromInteger contentLength)]
        304 -> toOutputType Null
        _   -> failed Unknown

-- | Get the specified design document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#get--db-_design-ddoc API documentation>
--
-- If the specified DocRev matches, returns a JSON Null, otherwise a
-- JSON value for the document.
--
-- Status: __Broken__
get :: (FromJSON a, MonadIO m) => DocGetDoc -> DocId -> Maybe DocRev -> Context -> m (CouchResult a)
get = Base.get "_design"

-- | Create or replace the specified design document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#put--db-_design-ddoc API documentation>
--
-- Returns a JSON value.
--
-- Status: __Broken__
put :: (FromJSON a, MonadIO m, ToJSON b) => DocPut -> DocId -> Maybe DocRev -> b -> Context -> m (CouchResult a)
put = Base.put "_design"

-- | Delete the specified design document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#delete--db-_design-ddoc API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
delete :: (FromJSON a, MonadIO m) => DocPut -> DocId -> Maybe DocRev -> Context -> m (CouchResult a)
delete = Base.delete "_design"

-- | Copy the specified design document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#copy--db-_design-ddoc API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
copy :: (FromJSON a, MonadIO m) => DocPut -> DocId -> Maybe DocRev -> DocId -> Context -> m (CouchResult a)
copy = Base.copy "_design"

-- | Get information on a design document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/ddoc/common.html#get--db-_design-ddoc-_info documentation>
--
-- The returned data is variable enough we content ourselves with just
-- a 'Value' for you to take apart.
--
-- Status: __Complete__
info :: (FromJSON a, MonadIO m) => DocId -> Context -> m (CouchResult a)
info doc =
  standardRequest request
  where
    request = do
      Base.accessBase "_design" doc Nothing
      addPath "_info"

viewBase :: ViewParams -> DocId -> DocId -> RequestBuilder ()
viewBase params doc view = do
      Base.accessBase "_design" doc Nothing
      addPath "_view"
      selectDoc view
      setQueryParam $ toQueryParameters params

-- | Get a list of all database documents.
--
-- <http://docs.couchdb.org/en/1.6.1/api/database/bulk-api.html#get--db-_all_docs API documentation>
--
-- The returned data is variable enough we content ourselves with just
-- a 'Value' for you to take apart.
--
-- Status: __Complete__
allDocs :: (FromJSON a, MonadIO m) => ViewParams -> DocId -> DocId -> Context -> m (CouchResult a)
allDocs params doc view =
  standardRequest $ viewBase params doc view

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
someDocs :: (FromJSON a, MonadIO m) => ViewParams -> DocId -> DocId -> [DocId] -> Context -> m (CouchResult a)
someDocs params doc view ids =
  standardRequest request
  where
    request = do
      setMethod "POST"
      viewBase params doc view
      let docs = object [("keys", toJSON ids)]
      setJsonBody docs
