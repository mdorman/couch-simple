{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Module      : Database.Couch.Explicit.Server
Description : Server-oriented requests to CouchDB, with explicit parameters
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has been made to keep names of types or functions from clashing with obvious or otherwise commonly-used names, or even other modules within this package.

The functions here are derived from (and presented in the same order as) the <http://docs.couchdb.org/en/1.6.1/api/server/index.html Server API documentation>.  For each function, we attempt to link back to the original documentation, as well as make a notation as to how complete and correct we feel our implementation is.

Each function takes a 'Database.Couch.Types.Context' as its final parameter, and returns a 'Database.Couch.Types.Result'.

-}

module Database.Couch.Explicit.Server where

import           Control.Monad                 (return)
import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON)
import           Data.Function                 (($))
import           Data.Int                      (Int)
import           Data.Maybe                    (Maybe (Just))
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import           Database.Couch.Internal       (standardRequest)
import           Database.Couch.RequestBuilder (addPath, addQueryParam,
                                                setMethod, setQueryParam)
import           Database.Couch.Types          (Context, DbUpdates, Result,
                                                toQueryParameters)
import           GHC.Err                       (undefined)
import           Text.Show                     (show)

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get-- Get most basic meta-information>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Server.meta ctx

Status: __Complete__ -}
meta :: (FromJSON a, MonadIO m)
     => Context
     -> m (Result a)
meta =
  standardRequest request
  where
    -- This is actually the default request
    request =
      return ()

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_active_tasks Get a list of active tasks>

The return value is a list of objects whose fields often vary, so it is easily decoded as a 'Data.List.List' of 'Data.Aeson.Value':

>>> value :: Result [Value] <- Server.activeTasks ctx

Status: __Complete__ -}
activeTasks :: (FromJSON a, MonadIO m)
            => Context
            -> m (Result a)
activeTasks =
  standardRequest request
  where
    request =
      addPath "_active_tasks"

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_all_dbs Get a list of all databases>

The return value is a list of database names, so it is easily decoded into a 'Data.List.List' of 'Text':

>>> value :: Result [Text] <- Server.allDbs ctx

Status: __Complete__ -}
allDbs :: (FromJSON a, MonadIO m)
       => Context
       -> m (Result a)
allDbs =
  standardRequest request
  where
    request =
      addPath "_all_dbs"

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_db_updates Get a list of all database events>

This call does not yet stream out results, so it's functionality is limited.

The return value is a list of database update events, so it is easily decoded into a 'Data.List.List' of 'Data.Aeson.Value':

>>> value :: Result [Value] <- Server.dbUpdates ctx

Status: __Limited__ -}
dbUpdates :: (FromJSON a, MonadIO m) => DbUpdates -> Context -> m (Result a)
dbUpdates param =
  standardRequest request
  where
    request = do
      addPath "_db_updates"
      setQueryParam $ toQueryParameters param

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_log Get the log output of the server>

This call doesn't return a JSON result, so we're deferring support for the moment.

Status: __Unimplemented__ -}
log :: MonadIO m => Context -> m (Result Text)
log = undefined

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#post--_replicate Administer replication for databases on the server>

We do not yet have a structure for specifying parameters to this call, so we're deferring support for the moment

Status: __Unimplemented__ -}
replicate :: (FromJSON a, MonadIO m)
          => Context
          -> m (Result a)
replicate =
  standardRequest request
  where
    request = do
      setMethod "POST"
      addPath "_replicate"

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#post--_restart Restart the server>

The return value is an object that should only contain a single key "ok", so it is easily decoded into a 'Data.Bool.Bool' with our 'asBool' combinator:

>>> value :: Result Bool <- Server.restart ctx >>= asBool

Status: __Complete__ -}
restart :: (FromJSON a, MonadIO m)
        => Context
        -> m (Result a)
restart =
  standardRequest request
  where
    request = do
      setMethod "POST"
      addPath "_restart"

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_stats Get server statistics>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Server.stats ctx

Status: __Complete__ -}
stats :: (FromJSON a, MonadIO m)
      => Context
      -> m (Result a)
stats =
  standardRequest request
  where
    request =
      addPath "_stats"

{- | <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_uuids Get a batch of UUIDs>

The return value is a list of strings representing UUIDs, so it is easily decoded as a 'Data.List.List' of 'Data.UUID.UUID' with our 'asUUID' combinator:

>>> value :: Result [UUID] <- Server.stats ctx >>= asUUID

Status: __Complete__ -}
uuids :: (FromJSON a, MonadIO m)
      => Int -- ^ How many 'UUID's to retrieve
      -> Context
      -> m (Result a)
uuids count =
  standardRequest request
  where
    request = do
      addPath "_uuids"
      addQueryParam [("count", Just $ fromString $ show count)]
