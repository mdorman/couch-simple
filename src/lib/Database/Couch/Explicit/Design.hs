{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Explicit.Design
Description : Design Document-oriented requests to CouchDB, with explicit parameters
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has been made to keep names of types or functions from clashing with obvious or otherwise commonly-used names, or even other modules within this package.

The functions here are derived from (and presented in the same order as) the <http://docs.couchdb.org/en/1.6.1/api/ddoc/common.html Design Document API documentation>.  For each function, we attempt to link back to the original documentation, as well as make a notation as to how complete and correct we feel our implementation is.

Each function takes a 'Database.Couch.Types.Context'---which, among other things, holds the name of the database---as its final parameter, and returns a 'Database.Couch.Types.Result'.

-}

module Database.Couch.Explicit.Design where

import           Control.Monad.IO.Class          (MonadIO)
import           Data.Aeson                      (FromJSON, ToJSON, object,
                                                  toJSON)
import           Data.Function                   (($))
import           Data.Maybe                      (Maybe (Nothing))
import qualified Database.Couch.Explicit.DocBase as Base (accessBase, copy,
                                                          delete, get, meta,
                                                          put)
import           Database.Couch.Internal         (standardRequest)
import           Database.Couch.RequestBuilder   (RequestBuilder, addPath,
                                                  selectDoc, setJsonBody,
                                                  setMethod, setQueryParam)
import           Database.Couch.Types            (Context, DocId, DocRev,
                                                  ModifyDoc, Result,
                                                  RetrieveDoc, ViewParams,
                                                  toQueryParameters)

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#head--db-_design-ddoc Get the size and revision of the specified design document>

The return value is an object that should only contain the keys "rev" and "size", that can be easily parsed into a pair of (DocRev, Int):

>>> (,) <$> (getKey "rev" >>= toOutputType) <*> (getKey "size" >>= toOutputType)

If the specified DocRev matches, returns a JSON Null, otherwise a JSON value for the document.

Status: __Complete__ -}
meta :: (FromJSON a, MonadIO m)
     => RetrieveDoc -- ^ Parameters for the HEAD request
     -> DocId -- ^ The ID of the design document
     -> Maybe DocRev -- ^ A desired revision
     -> Context
     -> m (Result a)
meta = Base.meta "_design"

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#get--db-_design-ddoc Get the specified design document>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Design.get "pandas" Nothing ctx

If the specified DocRev matches, returns a JSON Null, otherwise a JSON value for the document.

Status: __Complete__ -}
get :: (FromJSON a, MonadIO m)
    => RetrieveDoc -- ^ Parameters for the HEAD request
    -> DocId -- ^ The ID of the design document
    -> Maybe DocRev -- ^ A desired revision
    -> Context
    -> m (Result a)
get = Base.get "_design"

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#put--db-_design-ddoc Create or replace the specified design document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Design.put modifyDoc "pandas" Nothing SomeValue ctx >>= asBool

Status: __Complete__ -}
put :: (FromJSON a, MonadIO m, ToJSON b)
    => ModifyDoc -- ^ Parameters for the request
    -> DocId -- ^ The ID of the design document
    -> Maybe DocRev -- ^ A desired revision
    -> b
    -> Context
    -> m (Result a)
put = Base.put "_design"

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#delete--db-_design-ddoc Delete the specified design document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Design.delete modifyDoc "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
delete :: (FromJSON a, MonadIO m)
       => ModifyDoc -- ^ Parameters for the request
       -> DocId -- ^ The ID of the design document
       -> Maybe DocRev -- ^ A desired revision
       -> Context
       -> m (Result a)
delete = Base.delete "_design"

{- | <http://docs.couchdb.org/en/1.6.1/api/document/common.html#copy--db-_design-ddoc Copy the specified design document>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Design.delete modifyDoc "pandas" Nothing ctx >>= asBool

Status: __Complete__ -}
copy :: (FromJSON a, MonadIO m)
     => ModifyDoc -- ^ Parameters for the request
     -> DocId -- ^ The ID of the design document
     -> Maybe DocRev -- ^ A desired revision
     -> DocId
     -> Context
     -> m (Result a)
copy = Base.copy "_design"

{- | <http://docs.couchdb.org/en/1.6.1/api/ddoc/common.html#get--db-_design-ddoc-_info Get information on a design document>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Design.info "pandas" ctx

Status: __Complete__ -}
info :: (FromJSON a, MonadIO m)
     => DocId -- ^ The ID of the design document
     -> Context
     -> m (Result a)
info doc =
  standardRequest request
  where
    request = do
      Base.accessBase "_design" doc Nothing
      addPath "_info"

{- | <http://docs.couchdb.org/en/1.6.1/api/ddoc/views.html#get--db-_design-ddoc-_view-view Get a list of all database documents>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Design.allDocs viewParams "pandas" "counter" ctx

Status: __Complete__ -}
allDocs :: (FromJSON a, MonadIO m)
        => ViewParams -- ^ Parameters for the request
        -> DocId -- ^ The ID of the design document
        -> DocId -- ^ The ID of the view
        -> Context
        -> m (Result a)
allDocs params doc view =
  standardRequest $ viewBase params doc view

{- | <http://docs.couchdb.org/en/1.6.1/api/ddoc/views.html#post--db-_design-ddoc-_view-view Get a list of some database documents>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Design.someDocs viewParams "pandas" "counter" ["a", "b"] ctx

Status: __Complete__ -}
someDocs :: (FromJSON a, MonadIO m)
         => ViewParams -- ^ Parameters for the request
         -> DocId -- ^ The ID of the design document
         -> DocId -- ^ The ID of the view
         -> [DocId] -- ^ The IDs of the documents of interest
         -> Context
         -> m (Result a)
someDocs params doc view ids =
  standardRequest request
  where
    request = do
      setMethod "POST"
      viewBase params doc view
      let docs = object [("keys", toJSON ids)]
      setJsonBody docs

-- * Internal combinators

-- | Base bits for all view queries
viewBase :: ViewParams -> DocId -> DocId -> RequestBuilder ()
viewBase params doc view = do
      Base.accessBase "_design" doc Nothing
      addPath "_view"
      selectDoc view
      setQueryParam $ toQueryParameters params
