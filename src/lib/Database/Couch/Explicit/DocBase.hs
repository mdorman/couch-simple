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

This module is not intended to be used directly---it is used to construct a number of otherwise-similar modules, where the modules are primarily concerned with the existence (or not) of a path prefix for documents.

The functions here are effectively derived from (and presented in the same order as) the <http://docs.couchdb.org/en/1.6.1/api/document/common.html Document API documentation>, though we don't link back to the specific functions here, since they're not meant for direct use.

Each function takes a 'Database.Couch.Types.Context'---which, among other things, holds the name of the database---as its final parameter, and returns a 'Database.Couch.Types.Result'.

-}

module Database.Couch.Explicit.DocBase where

import           Control.Monad                 (return, unless)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Except    (throwE)
import           Data.Aeson                    (FromJSON, ToJSON,
                                                Value (Null, Number, String),
                                                object)
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
import           Database.Couch.ResponseParser (checkStatusCode,
                                                getContentLength, getDocRev,
                                                responseStatus, responseValue,
                                                toOutputType)
import           Database.Couch.Types          (Context, DocGetDoc, DocId,
                                                DocPut, DocRev, Error (Unknown),
                                                Result, reqDocId, reqDocRev,
                                                toHTTPHeaders,
                                                toQueryParameters, unwrapDocRev)
import           GHC.Num                       (fromInteger)
import           Network.HTTP.Types            (statusCode)

{- | Get the size and revision of the specified document

The return value is an object that should only contain the keys "rev" and "size", that can be easily parsed into a pair of (DocRev, Int):

>>> (,) <$> (getKey "rev" >>= toOutputType) <*> (getKey "size" >>= toOutputType)

If the specified DocRev matches, returns a JSON Null, otherwise a JSON value for the document.

Status: __Complete__ -}
meta :: (FromJSON a, MonadIO m)
     => ByteString -- ^ The prefix to use for the document
     -> DocGetDoc -- ^ Parameters for document retrieval
     -> DocId -- ^ The document ID
     -> Maybe DocRev -- ^ An optional document revision
     -> Context
     -> m (Result a)
meta prefix param doc rev =
  structureRequest request parse
  where
    request = do
      setMethod "HEAD"
      accessBase prefix doc rev
      setQueryParam $ toQueryParameters param
    parse = do
      -- Do our standard status code checks
      checkStatusCode
      -- And then handle 304 appropriately
      s <- responseStatus
      docRev <- getDocRev
      contentLength <- getContentLength
      case statusCode s of
        200 -> toOutputType $ object [("rev", String $ unwrapDocRev docRev), ("size", Number $ fromInteger contentLength)]
        304 -> toOutputType Null
        _   -> throwE Unknown

{- | Get the specified document

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- DocBase.get "prefix" "pandas" Nothing ctx

If the specified DocRev matches, returns a JSON Null, otherwise a JSON value for the document.

Status: __Complete__ -}
get :: (FromJSON a, MonadIO m)
    => ByteString -- ^ A prefix for the document ID
    -> DocGetDoc -- ^ Parameters for document retrieval
    -> DocId -- ^ The document ID
    -> Maybe DocRev -- ^ An optional document revision
    -> Context
    -> m (Result a)
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
        _   -> throwE Unknown

{- | Create or replace the specified document

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.put "prefix" docPut "pandas" Nothing SomeValue ctx >>= asBool

Status: __Complete__ -}
put :: (FromJSON a, MonadIO m, ToJSON b)
    => ByteString -- ^ A prefix for the document ID
    -> DocPut -- ^ The parameters for modifying the document
    -> DocId -- ^ The document ID
    -> Maybe DocRev -- ^ An optional document revision
    -> b -- ^ The document
    -> Context
    -> m (Result a)
put prefix param docid rev doc =
  standardRequest request
  where
    request = do
      setMethod "PUT"
      modBase prefix param docid rev
      setJsonBody doc

{- | Delete the specified document

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.delete "prefix" docPut "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
delete :: (FromJSON a, MonadIO m)
       => ByteString -- ^ A prefix for the document ID
       -> DocPut -- ^ The parameters for modifying the document
       -> DocId -- ^ The document ID
       -> Maybe DocRev -- ^ An optional document revision
       -> Context
       -> m (Result a)
delete prefix param docid rev =
  standardRequest request
  where
    request = do
      setMethod "DELETE"
      modBase prefix param docid rev

{- | Copy the specified document

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.delete "prefix" docPut "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
copy :: (FromJSON a, MonadIO m)
     => ByteString -- ^ A prefix for the document ID
     -> DocPut -- ^ The parameters for modifying the document
     -> DocId -- ^ The document ID
     -> Maybe DocRev -- ^ An optional document revision
     -> DocId -- ^ The destination document ID
     -> Context
     -> m (Result a)
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

-- | = Internal combinators

-- | Construct a path in a consistent fashion
docPath :: ByteString -- ^ A prefix for the document ID
        -> DocId -- ^ The document ID
        -> RequestBuilder ()
docPath prefix docid = do
  selectDb
  unless (null prefix) $
    addPath prefix
  selectDoc docid

-- | All retrievals want to allow 304s
accessBase :: ByteString -- ^ A prefix for the document ID
           -> DocId -- ^ The document ID
           -> Maybe DocRev -- ^ An optional document revision
           -> RequestBuilder ()
accessBase prefix docid rev = do
  docPath prefix docid
  maybe (return ()) (setHeaders . return . ("If-None-Match" ,) . reqDocRev) rev

-- | All modifications want to allow conflict recognition and parameters
modBase :: ByteString -- ^ A prefix for the document ID
        -> DocPut -- ^ The parameters for modifying the document
        -> DocId -- ^ The document ID
        -> Maybe DocRev -- ^ An optional document revision
        -> RequestBuilder ()
modBase prefix param docid rev = do
  docPath prefix docid
  maybe (return ()) (setHeaders . return . ("If-Match" ,) . reqDocRev) rev
  setHeaders $ toHTTPHeaders param
  setQueryParam $ toQueryParameters param
