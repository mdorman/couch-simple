{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Explicit.Doc
Description : Document-oriented requests to CouchDB
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

module Database.Couch.Explicit.Doc where

import           Control.Monad                 (return)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON, ToJSON,
                                                Value (Null, Number), object)
import           Data.Function                 (($), (.))
import           Data.Maybe                    (Maybe, maybe)
import           Database.Couch.Internal       (standardRequest,
                                                structureRequest)
import           Database.Couch.RequestBuilder (RequestBuilder, selectDb,
                                                selectDoc, setHeaders,
                                                setJsonBody, setMethod,
                                                setQueryParam)
import           Database.Couch.ResponseParser (checkStatusCode, failed,
                                                getContentLength, getDocRev,
                                                responseStatus, responseValue,
                                                toOutputType)
import           Database.Couch.Types          (Context, CouchError (Unknown),
                                                CouchResult, DocGetDoc, DocId,
                                                DocPut, DocRev, reqDocRev,
                                                toHTTPHeaders,
                                                toQueryParameters, unwrapDocRev)
import           GHC.Num                       (fromInteger)
import           Network.HTTP.Types            (statusCode)

-- Common setup for the next Few items
docAccessBase :: DocId -> Maybe DocRev -> RequestBuilder ()
docAccessBase doc rev = do
  selectDb
  selectDoc doc
  maybe (return ()) (setHeaders . return . ("If-None-Match" ,) . reqDocRev) rev

-- | Get the size and revision of the specified document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#head--db-docid API documentation>
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
      docAccessBase doc rev
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

-- | Get the specified document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#get--db-docid API documentation>
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
      setMethod "GET"
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

modBase :: DocPut -> DocId -> Maybe DocRev -> RequestBuilder ()
modBase param docid rev = do
  selectDb
  selectDoc docid
  maybe (return ()) (setHeaders . return . ("If-Match" ,) . reqDocRev) rev
  setHeaders $ toHTTPHeaders param
  setQueryParam $ toQueryParameters param

-- | Create or replace the specified document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#put--db-docid API documentation>
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

-- | Delete the specified document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#delete--db-docid API documentation>
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
