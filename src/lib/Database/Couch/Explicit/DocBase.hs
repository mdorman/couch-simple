{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Explicit.Internal
Description : Parameterized document-oriented requests to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is not intended to be used directly---it is used to
constructo a number of otherwise-similar modules.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/local/index.html.

-}

module Database.Couch.Explicit.DocBase where

import           Control.Monad                 (return, unless)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON, ToJSON, Value (Null))
import           Data.ByteString               (ByteString, null)
import           Data.Function                 (($), (.))
import           Data.Maybe                    (Maybe, maybe)
import           Data.Monoid                   ((<>))
import           Database.Couch.Internal       (standardRequest,
                                                structureRequest)
import           Database.Couch.RequestBuilder (RequestBuilder, addPath,
                                                selectDb, selectDoc, setHeaders,
                                                setJsonBody, setMethod,
                                                setQueryParam)
import           Database.Couch.ResponseParser (checkStatusCode, failed,
                                                responseStatus, responseValue,
                                                toOutputType)
import           Database.Couch.Types          (Context, DocGetDoc, DocId,
                                                DocPut, DocRev, Error (Unknown),
                                                Result, reqDocId, reqDocRev,
                                                toHTTPHeaders,
                                                toQueryParameters)
import           Network.HTTP.Types            (statusCode)

-- Everything sets up the path the same
docPath :: ByteString -> DocId -> RequestBuilder ()
docPath prefix docid = do
  selectDb
  unless (null prefix) $
    addPath prefix
  selectDoc docid

-- All retrievals want to allow 304s
accessBase :: ByteString -> DocId -> Maybe DocRev -> RequestBuilder ()
accessBase prefix docid rev = do
  docPath prefix docid
  maybe (return ()) (setHeaders . return . ("If-None-Match" ,) . reqDocRev) rev

-- | Get the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#get--db-_local-docid API documentation>
--
-- If the specified DocRev matches, returns a JSON Null, otherwise a
-- JSON value for the document.
--
-- Status: __Broken__
get :: (FromJSON a, MonadIO m) => ByteString -> DocGetDoc -> DocId -> Maybe DocRev -> Context -> m (Result a)
get prefix param doc rev =
  structureRequest request parse
  where
    request = do
      accessBase prefix doc rev
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
modBase :: ByteString -> DocPut -> DocId -> Maybe DocRev -> RequestBuilder ()
modBase prefix param docid rev = do
  docPath prefix docid
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
put :: (FromJSON a, MonadIO m, ToJSON b) => ByteString -> DocPut -> DocId -> Maybe DocRev -> b -> Context -> m (Result a)
put prefix param docid rev doc =
  standardRequest request
  where
    request = do
      setMethod "PUT"
      modBase prefix param docid rev
      setJsonBody doc

-- | Delete the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#delete--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
delete :: (FromJSON a, MonadIO m) => ByteString -> DocPut -> DocId -> Maybe DocRev -> Context -> m (Result a)
delete prefix param docid rev =
  standardRequest request
  where
    request = do
      setMethod "DELETE"
      modBase prefix param docid rev

-- | Copy the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#copy--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
copy :: (FromJSON a, MonadIO m) => ByteString -> DocPut -> DocId -> Maybe DocRev -> DocId -> Context -> m (Result a)
copy prefix param source rev dest =
  standardRequest request
  where
    request = do
      setMethod "COPY"
      modBase prefix param source rev
      setHeaders [("Destination", destination)]
    destination =
      if null prefix
      then reqDocId dest
      else prefix <> "/" <> reqDocId dest
