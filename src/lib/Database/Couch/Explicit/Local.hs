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

-- | Get the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#get--db-_local-docid API documentation>
--
-- If the specified DocRev matches, returns a JSON Null, otherwise a
-- JSON value for the document.
--
-- Status: __Broken__
get :: (FromJSON a, MonadIO m) => DocGetDoc -> DocId -> Maybe DocRev -> Context -> m (Result a)
get = Base.get "_local"

-- | Create or replace the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#put--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Broken__
put :: (FromJSON a, MonadIO m, ToJSON b) => DocPut -> DocId -> Maybe DocRev -> b -> Context -> m (Result a)
put = Base.put "_local"

-- | Delete the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#delete--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
delete :: (FromJSON a, MonadIO m) => DocPut -> DocId -> Maybe DocRev -> Context -> m (Result a)
delete = Base.delete "_local"

-- | Copy the specified local document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/local.html#copy--db-_local-docid API documentation>
--
-- Returns a JSON value.
--
-- Status: __Complete__
copy :: (FromJSON a, MonadIO m) => DocPut -> DocId -> Maybe DocRev -> DocId -> Context -> m (Result a)
copy = Base.copy "_local"
