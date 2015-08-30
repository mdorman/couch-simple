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

import           Control.Monad.IO.Class          (MonadIO)
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Maybe                      (Maybe)
import qualified Database.Couch.Explicit.DocBase as Base (copy, delete, get,
                                                          put)
import           Database.Couch.Types            (Context, DocGetDoc, DocId,
                                                  DocPut, DocRev, Result)

{- | <http://docs.couchdb.org/en/1.6.1/api/local.html#get--db-_local-docid Get the specified local document>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Doc.get "pandas" Nothing ctx

If the specified DocRev matches, returns a JSON Null, otherwise a JSON value for the document.

Status: __Complete__ -}
get :: (FromJSON a, MonadIO m)
    => DocGetDoc -- ^ Parameters for document retrieval
    -> DocId -- ^ The document ID
    -> Maybe DocRev -- ^ An optional document revision
    -> Context
    -> m (Result a)
get = Base.get "_local"

{- | <http://docs.couchdb.org/en/1.6.1/api/local.html#put--db-_local-docid Create or replace the specified local document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.put docPut "pandas" Nothing SomeValue ctx >>= asBool

Status: __Complete__ -}
put :: (FromJSON a, MonadIO m, ToJSON b)
    => DocPut -- ^ Parameters for document modification
    -> DocId -- ^ The document ID
    -> Maybe DocRev -- ^ An optional document revision
    -> b -- ^ The document
    -> Context -> m (Result a)
put = Base.put "_local"

{- | <http://docs.couchdb.org/en/1.6.1/api/local.html#delete--db-_local-docid Delete the specified local document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.delete "prefix" docPut "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
delete :: (FromJSON a, MonadIO m)
       => DocPut -- ^ Parameters for document modification
       -> DocId -- ^ The document ID
       -> Maybe DocRev -- ^ An optional document revision
       -> Context
       -> m (Result a)
delete = Base.delete "_local"

{- | <http://docs.couchdb.org/en/1.6.1/api/local.html#copy--db-_local-docid Copy the specified local document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.delete "prefix" docPut "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
copy :: (FromJSON a, MonadIO m)
     => DocPut -- ^ Parameters for document modification
     -> DocId -- ^ The document ID
     -> Maybe DocRev -- ^ An optional document revision
     -> DocId -- ^ The destination document ID
     -> Context -> m (Result a)
copy = Base.copy "_local"
