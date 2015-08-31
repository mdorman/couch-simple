{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Explicit.Database
Description : Database-oriented requests to CouchDB, with explicit parameters
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has been made to keep names of types or functions from clashing with obvious or otherwise commonly-used names, or even other modules within this package.

The functions here are derived from (and presented in the same order as) the <http://docs.couchdb.org/en/1.6.1/api/database/index.html Database API documentation>.  For each function, we attempt to link back to the original documentation, as well as make a notation as to how complete and correct we feel our implementation is.

Each function takes a 'Database.Couch.Types.Context'---which, among other things, holds the name of the database---as its final parameter, and returns a 'Database.Couch.Types.Result'.

-}

module Database.Couch.Explicit.Database where

import           Control.Monad                 (return, when)
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Except    (throwE)
import           Data.Aeson                    (FromJSON, ToJSON,
                                                Value (Object), object, toJSON)
import           Data.Bool                     (Bool (True))
import           Data.Function                 (($), (.))
import           Data.Functor                  (fmap)
import           Data.HashMap.Strict           (fromList)
import           Data.Int                      (Int)
import           Data.Maybe                    (Maybe (Just), catMaybes,
                                                fromJust, isJust)
import           Data.Text                     (Text)
import           Data.Text.Encoding            (encodeUtf8)
import           Database.Couch.Internal       (standardRequest,
                                                structureRequest)
import           Database.Couch.RequestBuilder (RequestBuilder, addPath,
                                                selectDb, selectDoc, setHeaders,
                                                setJsonBody, setMethod,
                                                setQueryParam)
import           Database.Couch.ResponseParser (responseStatus, toOutputType)
import           Database.Couch.Types          (Context, DbAllDocs, DbBulkDocs,
                                                DbChanges, DocId, DocRevMap,
                                                Error (NotFound, Unknown),
                                                Result, ToQueryParameters,
                                                bdAllOrNothing, bdFullCommit,
                                                bdNewEdits, cLastEvent,
                                                toQueryParameters)
import           Network.HTTP.Types            (statusCode)

{- | <http://docs.couchdb.org/en/1.6.1/api/database/common.html#head--db Check that the requested database exists>

The return value is an object that should only contain a single key "ok", so it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Database.exists ctx >>= asBool

Status: __Complete__ -}
exists :: (FromJSON a, MonadIO m)
       => Context
       -> m (Result a)
exists =
  structureRequest request parse
  where
    request = do
      selectDb
      setMethod "HEAD"
    parse = do
      -- Check status codes by hand because we don't want 404 to be an error, just False
      s <- responseStatus
      case statusCode s of
        200 -> toOutputType $ object [("ok", toJSON True)]
        404 -> throwE NotFound
        _   -> throwE Unknown

{- | <http://docs.couchdb.org/en/1.6.1/api/database/common.html#get--db Get most basic meta-information>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.meta ctx

Status: __Complete__ -}
meta :: (FromJSON a, MonadIO m)
     => Context
     -> m (Result a)
meta =
  standardRequest request
  where
    request =
      selectDb

{- | <http://docs.couchdb.org/en/1.6.1/api/database/common.html#put--db Create a database>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.meta ctx

Status: __Complete__ -}
create :: (FromJSON a, MonadIO m)
       => Context
       -> m (Result a)
create =
  standardRequest request
  where
    request = do
      selectDb
      setMethod "PUT"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/common.html#delete--db Delete a database>

The return value is an object that should only contain a single key "ok", so it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Database.delete ctx >>= asBool

Status: __Complete__ -}
delete :: (FromJSON a, MonadIO m)
       => Context
       -> m (Result a)
delete =
  standardRequest request
  where
    request = do
      selectDb
      setMethod "DELETE"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/common.html#post--db Create a new document in a database>

The return value is an object that can hold "id" and "rev" keys, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Database.createDoc True someObject ctx >>= asBool

Status: __Complete__ -}
createDoc :: (FromJSON a, MonadIO m, ToJSON b)
          => Bool -- ^ Whether to create the document in batch mode
          -> b -- ^ The document to create
          -> Context
          -> m (Result a)
createDoc batch doc =
  standardRequest request
  where
    request = do
      selectDb
      setMethod "POST"
      when batch (setQueryParam [("batch", Just "ok")])
      setJsonBody doc

{- | <http://docs.couchdb.org/en/1.6.1/api/database/bulk-api.html#get--db-_all_docs Get a list of all database documents>

The return value is a list of objects whose fields often vary, so it is easily decoded as a 'Data.List.List' of 'Data.Aeson.Value':

>>> value :: Result [Value] <- Database.allDocs dbAllDocs ctx

Status: __Complete__ -}
allDocs :: (FromJSON a, MonadIO m)
        => DbAllDocs -- ^ Parameters governing retrieval ('Database.Couch.Types.dbAllDocs' is an empty default)
        -> Context
        -> m (Result a)
allDocs =
  standardRequest . allDocsBase

{- | <http://docs.couchdb.org/en/1.6.1/api/database/bulk-api.html#post--db-_all_docs Get a list of some database documents>

The return value is a list of objects whose fields often vary, so it is easily decoded as a 'Data.List.List' of 'Data.Aeson.Value':

>>> value :: Result [Value] <- Database.someDocs ["a", "b", "c"] ctx

Status: __Complete__ -}
someDocs :: (FromJSON a, MonadIO m)
         => DbAllDocs -- ^ Parameters governing retrieval ('Database.Couch.Types.dbAllDocs' is an empty default)
         -> [DocId] -- ^ List of ids documents to retrieve
         -> Context
         -> m (Result a)
someDocs param ids =
  standardRequest request
  where
    request = do
      setMethod "POST"
      allDocsBase param
      let parameters = Object (fromList [("keys", toJSON ids)])
      setJsonBody parameters

{- | <http://docs.couchdb.org/en/1.6.1/api/database/bulk-api.html#post--db-_bulk_docs Create or update a list of documents>

The return value is a list of objects whose fields often vary, so it is easily decoded as a 'Data.List.List' of 'Data.Aeson.Value':

>>> value :: Result [Value] <- Database.bulkDocs dbBulkDocs ["a", "b", "c"] ctx

Status: __Complete__ -}
bulkDocs :: (FromJSON a, MonadIO m, ToJSON a)
         => DbBulkDocs -- ^ Parameters coverning retrieval ('Database.Couch.Types.dbBulkDocs' is an empty default)
         -> [a] -- ^ List of documents to add or update
         -> Context
         -> m (Result a)
bulkDocs param docs =
  standardRequest request
  where
    request = do
      setMethod "POST"
      -- TODO: We need a way to set a header when we have a value for it [refactor]
      when (isJust $ bdFullCommit param)
        (setHeaders
           [("X-Couch-Full-Commit", if fromJust $ bdFullCommit param
                                      then "true"
                                      else "false")])
      selectDb
      addPath "_bulk_docs"
      -- TODO: We need a way to construct a json body from parameters [refactor]
      let parameters = Object
                         ((fromList . catMaybes)
                            [ Just ("docs", toJSON docs)
                            , boolToParam "all_or_nothing" bdAllOrNothing
                            , boolToParam "new_edits" bdNewEdits
                            ])
      setJsonBody parameters
    boolToParam k s = do
      v <- s param
      return
        (k, if v
              then "true"
              else "false")

{- | <http://docs.couchdb.org/en/1.6.1/api/database/changes.html#get--db-_changes Get a list of all document modifications>

This call does not stream out results; so while it allows you to specify parameters for streaming, it's a dirty, dirty lie.

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.changes ctx

Status: __Limited__ -}
changes :: (FromJSON a, MonadIO m)
        => DbChanges -- ^ Arguments governing changes contents ('Database.Couch.Types.dbChanges' is an empty default)
        -> Context
        -> m (Result a)
changes param =
  standardRequest request
  where
    request = do
      -- TODO: We need a way to set a header when we have a value for it [refactor]
      when (isJust $ cLastEvent param)
        (setHeaders [("Last-Event-Id", encodeUtf8 . fromJust $ cLastEvent param)])
      selectDb
      addPath "_changes"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_compact Compact a database>

The return value is an object that should only contain a single key "ok", so it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Database.compact ctx >>= asBool

Status: __Complete__ -}
compact :: (FromJSON a, MonadIO m)
        => Context
        -> m (Result a)
compact =
  standardRequest compactBase

{- | <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_compact-ddoc Compact the views attached to a particular design document>

The return value is an object that should only contain a single key "ok", so it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Database.compactDesignDoc "ddoc" ctx >>= asBool

Status: __Complete__ -}
compactDesignDoc :: (FromJSON a, MonadIO m)
                 => DocId -- ^ The 'DocId' of the design document to compact
                 -> Context
                 -> m (Result a)
compactDesignDoc doc =
  standardRequest request
  where
    request = do
      compactBase
      selectDoc doc

{- | <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_ensure_full_commit Ensure that all changes to the database have made it to disk>

The return value is an object that can hold an "instance_start_time" key, but if you don't need those values, it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Database.sync ctx >>= asBool

Status: __Complete__ -}
sync :: (FromJSON a, MonadIO m)
     => Context
     -> m (Result a)
sync =
  standardRequest request
  where
    request = do
      setMethod "POST"
      selectDb
      addPath "_ensure_full_commit"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/compact.html#post--db-_view_cleanup Cleanup any stray view definitions>

The return value is an object that should only contain a single key "ok", so it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Database.cleanup ctx >>= asBool

Status: __Complete__ -}
cleanup :: (FromJSON a, MonadIO m)
        => Context
        -> m (Result a)
cleanup =
  standardRequest request
  where
    request = do
      setMethod "POST"
      selectDb
      addPath "_view_cleanup"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/security.html#get--db-_security Get security information for database>

The return value is an object that has with a standard set of fields ("admin" and "members" keys, which each contain "users" and "roles"), the system does not prevent you from adding (and even using in validation functions) additional fields, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.getSecurity ctx

Status: __Complete__ -}
getSecurity :: (FromJSON a, MonadIO m)
            => Context
            -> m (Result a)
getSecurity =
  standardRequest securityBase

{- | <http://docs.couchdb.org/en/1.6.1/api/database/security.html#post--db-_security Set security information for database>

The input value is an object that has with a standard set of fields ("admin" and "members" keys, which each contain "users" and "roles"), but the system does not prevent you from adding (and even using in validation functions) additional fields, so we don't specify a specific type, and you can roll your own:

The return value is an object that should only contain a single key "ok", so it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Value <- Database.setSecurity (object [("users", object [("harry")])]) ctx >>= asBool

Status: __Complete__ -}
setSecurity :: (FromJSON b, MonadIO m, ToJSON a)
            => a -- ^ The security document content
            -> Context
            -> m (Result b)
setSecurity doc =
  standardRequest request
  where
    request = do
      setMethod "PUT"
      securityBase
      setJsonBody doc

{- | <http://docs.couchdb.org/en/1.6.1/api/database/temp-views.html#post--db-_temp_view Create a temporary view>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.tempView "function (doc) { emit (1); }" (Just "_count") Nothing ctx

Status: __Complete__ -}
tempView :: (FromJSON a, MonadIO m)
         => Text -- ^ The text of your map function
         -> Maybe Text -- ^ The text of your optional reduce function
         -> Context
         -> m (Result a)
tempView map reduce =
  standardRequest request
  where
    request = do
      setMethod "POST"
      selectDb
      -- TODO: We need a way to construct a json body from parameters [refactor]
      let parameters = Object
                         (fromList $ catMaybes
                                       [ Just ("map", toJSON map)
                                       , fmap (("reduce",) . toJSON) reduce
                                       ])
      addPath "_temp_view"
      setJsonBody parameters

{- | <http://docs.couchdb.org/en/1.6.1/api/database/misc.html#post--db-_purge Purge document revisions from the database>

The return value is an object with two fields "purge_seq" and "purged", which contains an object with no fixed keys, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.purge $ DocRevMap [(DocId "junebug", [DocRev "1-1"])] Nothing ctx

However, the content of "purged" is effectively a 'Database.Couch.Types.DocRevMap', so the output can be parsed into an (Int, DocRevMap) pair using:

>>> (,) <$> (getKey "purge_seq" >>= toOutputType) <*> (getKey "purged" >>= toOutputType)

Status: __Complete__ -}
purge :: (FromJSON a, MonadIO m)
      => DocRevMap -- ^ A 'Database.Couch.Types.DocRevMap' of documents and versions to purge
      -> Context
      -> m (Result a)
purge docRevs =
  standardRequest request
  where
    request = do
      docRevBase docRevs
      addPath "_purge"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/misc.html#post--db-_missing_revs Find document revisions not present in the database>

The return value is an object with one field "missed_revs", which contains an object with no fixed keys, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.missingRevs $ DocRevMap [(DocId "junebug", [DocRev "1-1"])] ctx

However, the content of "missed_revs" is effectively a 'Database.Couch.Types.DocRevMap', so it can be parsed into a 'Database.Couch.Types.DocRevMap' using:

>>> getKey "missed_revs" >>= toOutputType

Status: __Complete__ -}
missingRevs :: (FromJSON a, MonadIO m)
            => DocRevMap -- ^ A 'Database.Couch.Types.DocRevMap' of documents and versions available
            -> Context
            -> m (Result a)
missingRevs docRevs =
  standardRequest request
  where
    request = do
      docRevBase docRevs
      addPath "_missing_revs"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/misc.html#post--db-_revs_diff Find document revisions not present in the database>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Database.revsDiff $ DocRevMap [(DocId "junebug", [DocRev "1-1"])] ctx

Status: __Complete__ -}
revsDiff :: (FromJSON a, MonadIO m)
         => DocRevMap -- ^ A 'Database.Couch.Types.DocRevMap' of documents and versions available
         -> Context
         -> m (Result a)
revsDiff docRevs =
  standardRequest request
  where
    request = do
      docRevBase docRevs
      addPath "_revs_diff"

{- | <http://docs.couchdb.org/en/1.6.1/api/database/misc.html#get--db-_revs_limit Get the revision limit setting>

The return value is a JSON numeric value that can easily be decoded to an 'Int':

>>> value :: Result Integer <- Database.getRevsLimit ctx

Status: __Complete__ -}
getRevsLimit :: (FromJSON a, MonadIO m) => Context -> m (Result a)
getRevsLimit =
  standardRequest revsLimitBase

{- | <http://docs.couchdb.org/en/1.6.1/api/database/misc.html#put--db-_revs_limit Set the revision limit>

Status: __Complete__ -}
setRevsLimit :: (FromJSON a, MonadIO m)
             => Int -- ^ The value at which to set the limit
             -> Context
             -> m (Result a)
setRevsLimit limit =
  standardRequest request
  where
    request = do
      setMethod "PUT"
      revsLimitBase
      setJsonBody limit

-- * Internal combinators

-- | Base bits for all _all_docs requests
allDocsBase :: ToQueryParameters a => a -> RequestBuilder ()
allDocsBase param = do
  selectDb
  addPath "_all_docs"
  setQueryParam $ toQueryParameters param

-- | Base bits for all our _compact requests
compactBase :: RequestBuilder ()
compactBase = do
  setMethod "POST"
  selectDb
  addPath "_compact"

-- | Base bits for our revision examination functions
docRevBase :: ToJSON a => a -> RequestBuilder ()
docRevBase docRevs = do
  setMethod "POST"
  selectDb
  let parameters = toJSON docRevs
  setJsonBody parameters

-- | Base bits for our revisions limit functions
revsLimitBase :: RequestBuilder ()
revsLimitBase = do
  selectDb
  addPath "_revs_limit"

-- | Base bits for our security functions
securityBase :: RequestBuilder ()
securityBase = do
  selectDb
  addPath "_security"
