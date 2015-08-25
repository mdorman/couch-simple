{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Module      : Database.Couch.Explicit.Configuration
Description : Requests for handling configuration of CouchDB, with explicit parameters
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has
been made to keep names of types or functions from clashing with
obvious or otherwise commonly-used names.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/server/configuration.html.

-}

module Database.Couch.Explicit.Configuration where

import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Database.Couch.Internal       (standardRequest)
import           Database.Couch.RequestBuilder (RequestBuilder, addPath,
                                                selectDoc, setJsonBody,
                                                setMethod)
import           Database.Couch.Types          (Context, DocId, Result)

-- | Get the configuration for the overall server.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config API documentation>
--
-- Returns a (structured) JSON Value.
--
-- Status: __Complete__
server :: (FromJSON a, MonadIO m) => Context -> m (Result a)
server =
  standardRequest request
  where
    request =
      addPath "_config"

-- | Get the configuration for one section.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns a (structured) JSON Value.
--
-- Status: __Complete__
section :: (FromJSON a, MonadIO m) => DocId -> Context -> m (Result a)
section s =
  standardRequest request
  where
    request = do
      addPath "_config"
      selectDoc s

configPath :: DocId -> DocId -> RequestBuilder ()
configPath s k = do
  addPath "_config"
  selectDoc s
  selectDoc k

-- | Get the configuration for one item.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns the JSON Value.
--
-- Status: __Complete__
getValue :: (FromJSON a, MonadIO m) => DocId -> DocId -> Context -> m (Result a)
getValue s k =
  standardRequest request
  where
    request =
      configPath s k

-- | Set the configuration value for one item.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns the previous JSON Value.
--
-- Status: __Complete__
setValue :: (ToJSON a, FromJSON b, MonadIO m) => DocId -> DocId -> a -> Context -> m (Result b)
setValue s k v =
  standardRequest request
  where
    request = do
      setMethod "PUT"
      configPath s k
      setJsonBody v

-- | Remove the configuration value for one item.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns the previous JSON Value.
--
-- Status: __Complete__
delValue :: (FromJSON a, MonadIO m) => DocId -> DocId -> Context -> m (Result a)
delValue s k =
  standardRequest request
  where
    request = do
      setMethod "DELETE"
      configPath s k
