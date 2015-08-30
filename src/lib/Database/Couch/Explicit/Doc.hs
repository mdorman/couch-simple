{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Explicit.Doc
Description : Document-oriented requests to CouchDB, with explicit parameters
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has been made to keep names of types or functions from clashing with obvious or otherwise commonly-used names, or even other modules within this package.

The functions here are derived from (and presented in the same order as) the <http://docs.couchdb.org/en/1.6.1/api/document/common.html Document API documentation>.  For each function, we attempt to link back to the original documentation, as well as make a notation as to how complete and correct we feel our implementation is.

Each function takes a 'Database.Couch.Types.Context'---which, among other things, holds the name of the database---as its final parameter, and returns a 'Database.Couch.Types.Result'.

-}

module Database.Couch.Explicit.Doc where

import           Control.Monad.IO.Class          (MonadIO)
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Maybe                      (Maybe)
import           Data.Monoid                     (mempty)
import qualified Database.Couch.Explicit.DocBase as Base (copy, delete, get,
                                                          put)
import           Database.Couch.Types            (Context, DocGetDoc, DocId,
                                                  DocPut, DocRev, Result)

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#head--db-docid Get the size and revision of the specified document>

The return value is an object that should only contain the keys "rev" and "size", that can be easily parsed into a pair of (DocRev, Int):

>>> (,) <$> (getKey "rev" >>= toOutputType) <*> (getKey "size" >>= toOutputType)

If the specified DocRev matches, returns a JSON Null, otherwise a JSON value for the document.

Status: __Complete__ -}
meta :: (FromJSON a, MonadIO m)
     => DocGetDoc -- ^ Parameters for document retrieval
     -> DocId -- ^ The document ID
     -> Maybe DocRev -- ^ An optional document revision
     -> Context
     -> m (Result a)
meta = Base.get mempty

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#get--db-docid Get the specified document>

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
get = Base.get mempty

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#put--db-docid Create or replace the specified document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.put docPut "pandas" Nothing SomeValue ctx >>= asBool

Status: __Complete__ -}
put :: (FromJSON a, MonadIO m, ToJSON b)
    => DocPut -- ^ Parameters for modifying document
    -> DocId -- ^ The document ID
    -> Maybe DocRev -- ^ An optional document revision
    -> b -- ^ The document
    -> Context
    -> m (Result a)
put = Base.put mempty

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#delete--db-docid Delete the specified document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.delete "prefix" docPut "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
delete :: (FromJSON a, MonadIO m)
       => DocPut -- ^ Parameters for modifying document
       -> DocId -- ^ The document ID
       -> Maybe DocRev -- ^ An optional document revision
       -> Context
       -> m (Result a)
delete = Base.delete mempty

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#copy--db-docid Copy the specified document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- DocBase.delete "prefix" docPut "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
copy :: (FromJSON a, MonadIO m)
     => DocPut -- ^ Parameters for modifying document
     -> DocId -- ^ The document ID
     -> Maybe DocRev -- ^ An optional document revision
     -> DocId -- ^ The destination document Id
     -> Context
     -> m (Result a)
copy = Base.copy mempty
