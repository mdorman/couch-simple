{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Explicit.Local
Description : Local document-oriented requests to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has
been made to keep names of types or functions from clashing with
obvious or otherwise commonly-used names.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/local/index.html.

-}

module Database.Couch.Explicit.Local where

import           Control.Monad                 (return)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON, ToJSON, Value (Null))
import           Data.Function                 (($), (.))
import           Data.Maybe                    (Maybe, maybe)
import           Database.Couch.Internal       (standardRequest,
                                                structureRequest)
import           Database.Couch.RequestBuilder (RequestBuilder, addPath,
                                                selectDb, selectDoc, setHeaders,
                                                setJsonBody, setMethod,
                                                setQueryParam)
import           Database.Couch.ResponseParser (checkStatusCode, failed,
                                                responseStatus, responseValue,
                                                toOutputType)
import           Database.Couch.Types          (Context, CouchError (Unknown),
                                                CouchResult, DocGetDoc, DocId,
                                                DocPut, DocRev, reqDocId,
                                                reqDocRev, toHTTPHeaders,
                                                toQueryParameters)
import           Network.HTTP.Types            (statusCode)

-- Everything sets up the path the same
docPath :: DocId -> RequestBuilder ()
docPath docid = do
  selectDb
  addPath "_local"
  selectDoc docid

-- All retrievals want to allow 304s
docAccessBase :: DocId -> Maybe DocRev -> RequestBuilder ()
docAccessBase docid rev = do
  docPath docid
  maybe (return ()) (setHeaders . return . ("If-None-Match" ,) . reqDocRev) rev

-- | Get the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#get--db-_local-docid API documentation>
--
-- If the specified DocRev matches, returns a JSON Null, otherwise a
-- JSON value for the document.
--
-- Status: __Broken__
get :: (FromJSON a, MonadIO m) => DocGetDoc -> DocId -> Maybe DocRev -> Context -> m (CouchResult a)
get param doc rev =
  structureRequest request parse
  where
    request = do
      docAccessBase doc rev
      setQueryParam $ toQueryParameters param
    parse = do
      -- Do our standard status code checks
      checkStatusCode
      -- And then handle 304 appropriately
      s <- responseStatus
      v <- responseValue
      case statusCode s of
        200 -> toOutputType v
        304 -> toOutputType Null
        _   -> failed Unknown

-- All modifications want to allow conflict recognition
modBase :: DocPut -> DocId -> Maybe DocRev -> RequestBuilder ()
modBase param docid rev = do
  docPath docid
  maybe (return ()) (setHeaders . return . ("If-Match" ,) . reqDocRev) rev
  setHeaders $ toHTTPHeaders param
  setQueryParam $ toQueryParameters param

-- | Create or replace the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#put--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Broken__
put :: (FromJSON a, MonadIO m, ToJSON b) => DocPut -> DocId -> Maybe DocRev -> b -> Context -> m (CouchResult a)
put param docid rev doc =
  standardRequest request
  where
    request = do
      setMethod "PUT"
      modBase param docid rev
      setJsonBody doc

-- | Delete the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#delete--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
delete :: (FromJSON a, MonadIO m) => DocPut -> DocId -> Maybe DocRev -> Context -> m (CouchResult a)
delete param docid rev =
  standardRequest request
  where
    request = do
      setMethod "DELETE"
      modBase param docid rev

-- | Copy the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#copy--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
copy :: (FromJSON a, MonadIO m) => DocPut -> DocId -> Maybe DocRev -> DocId -> Context -> m (CouchResult a)
copy param source rev dest =
  standardRequest request
  where
    request = do
      setMethod "COPY"
      modBase param source rev
      (setHeaders . return . ("Destination" ,) . reqDocId) dest
