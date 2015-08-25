{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Module      : Database.Couch.Explicit.Server
Description : Server-oriented requests to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has
been made to keep names of types or functions from clashing with
obvious or otherwise commonly-used names.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/server/index.html.

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

-- | Get most basic meta-information.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get-- API documentation>
--
-- The return value is easily decoded as a 'Value'.
--
-- Status: __Complete__
meta :: (FromJSON a, MonadIO m) => Context -> m (Result a)
meta =
  standardRequest request
  where
    -- This is actually the default request
    request =
      return ()

-- | Get a list of active tasks.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_active_tasks API documentation>
--
-- The return value is easily decoded into a 'List' of 'Value'.
--
-- Status: __Complete__
activeTasks :: (FromJSON a, MonadIO m) => Context -> m (Result a)
activeTasks =
  standardRequest request
  where
    request =
      addPath "_active_tasks"

-- | Get a list of all databases, cooked version.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_all_dbs API documentation>
--
-- The return value is easily decoded into a 'List' of 'Text'.
--
-- Status: __Complete__
allDbs :: (FromJSON a, MonadIO m) => Context -> m (Result a)
allDbs =
  standardRequest request
  where
    request =
      addPath "_all_dbs"

-- | Get a list of all database events.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_db_updates API documentation>
--
-- This call does not stream out results; therefore, it also doesn't
-- allow any specification of parameters for streaming.
--
-- The return value is easily decoded into a 'List' of 'Value'.
--
-- Status: __Limited__
dbUpdates :: (FromJSON a, MonadIO m) => DbUpdates -> Context -> m (Result a)
dbUpdates param =
  standardRequest request
  where
    request = do
      addPath "_db_updates"
      setQueryParam $ toQueryParameters param

-- | Get the log output of the server
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_log API documentation>
--
-- This call doesn't return a JSON result, so we're deferring support
-- for the moment.
--
-- Status: __Unimplemented__
log :: MonadIO m => Context -> m (Result Text)
log = undefined

-- | Administer replication for databases on the server
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#post--_replicate API documentation>
--
-- FIXME: We need to define arguments that get translated into the
-- appropriate query parameters, as well as combinators, localDb and
-- remoteDb that will produce appropriate Url parameters.
--
-- The return value is easily decoded into a 'Value'.
--
-- Status: __Broken__
replicate :: (FromJSON a, MonadIO m) => Context -> m (Result a)
replicate =
  standardRequest request
  where
    request = do
      setMethod "POST"
      addPath "_replicate"

-- | Restart the server
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#post--_restart API documentation>
--
-- The return value is easily decoded into a 'Boolean' using 'asBool'.
--
-- Status: __Complete__
restart :: (FromJSON a, MonadIO m) => Context -> m (Result a)
restart =
  standardRequest request
  where
    request = do
      setMethod "POST"
      addPath "_restart"

-- | Get server statistics
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_stats API documentation>
--
-- The return value is easily decoded into a 'Value'.
--
-- Status: __Complete__
stats :: (FromJSON a, MonadIO m) => Context -> m (Result a)
stats =
  standardRequest request
  where
    request =
      addPath "_stats"

-- | Get a batch of UUIDs
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/common.html#get--_uuids API documentation>
--
-- The return 'Value' is easily decoded into a list of 'UUID's using 'asUUID'.
--
-- Status: __Complete__
uuids :: (FromJSON a, MonadIO m) => Int -> Context -> m (Result a)
uuids count =
  standardRequest request
  where
    request = do
      addPath "_uuids"
      addQueryParam [("count", Just $ fromString $ show count)]
