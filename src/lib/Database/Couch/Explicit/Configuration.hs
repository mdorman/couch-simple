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

This module is intended to be @import qualified@.  /No attempt/ has been made to keep names of types or functions from clashing with obvious or otherwise commonly-used names, or even other modules within this package.

The functions here are derived from (and presented in the same order as) the <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html Server Configuration API documentation>.  For each function, we attempt to link back to the original documentation, as well as make a notation as to how complete and correct we feel our implementation is.

Each function takes a 'Database.Couch.Types.Context' as its final parameter, and returns a 'Database.Couch.Types.Result'.

-}

module Database.Couch.Explicit.Configuration where

import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Function                 (($), (.))
import           Database.Couch.Internal       (standardRequest)
import           Database.Couch.RequestBuilder (RequestBuilder, addPath,
                                                selectDoc, setJsonBody,
                                                setMethod)
import           Database.Couch.Types          (Context, DocId, Result)

{- | <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config Get the overall server configuration>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Configuration.server ctx

Status: __Complete__ -}
server :: (FromJSON a, MonadIO m)
       => Context
       -> m (Result a)
server =
  standardRequest configPath

{- | <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section Get the configuration for one section>

The return value is an object whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Configuration.section "auth" ctx

Status: __Complete__ -}
section :: (FromJSON a, MonadIO m)
        => DocId -- ^ Section name
        -> Context -> m (Result a)
section =
  standardRequest . sectionPath

{- | <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section Get the configuration for one item>

The return value is a JSON Value whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Configuration.getValue "auth" "ssl" ctx

Status: __Complete__-}
getValue :: (FromJSON a, MonadIO m)
         => DocId -- ^ Section name
         -> DocId -- ^ Key name
         -> Context -> m (Result a)
getValue s k =
  standardRequest $ itemPath s k

{- | <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section Set the configuration value for one item>

The value to set must be something that can be translated to JSON.

The return value is a JSON Value whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Configuration.setValue "auth" "ssl" Bool ctx

Status: __Complete__ -}
setValue :: (ToJSON a, FromJSON b, MonadIO m)
         => DocId -- ^ Section name
         -> DocId -- ^ Key name
         -> a -- ^ Value
         -> Context
         -> m (Result b)
setValue s k v =
  standardRequest request
  where
    request = do
      setMethod "PUT"
      itemPath s k
      setJsonBody v

{- | <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section Remove the configuration value for one item>

Returns the previous JSON Value.

The return value is a JSON Value whose fields often vary, so it is most easily decoded as a 'Data.Aeson.Value':

>>> value :: Result Value <- Configuration.delValue "auth" "ssl" ctx

Status: __Complete__ -}
delValue :: (FromJSON a, MonadIO m)
         => DocId -- ^ Section name
         -> DocId -- ^ Key name
         -> Context
         -> m (Result a)
delValue s k =
  standardRequest request
  where
    request = do
      setMethod "DELETE"
      itemPath s k

-- * Internal combinators

-- | Base path for all config requests
configPath :: RequestBuilder ()
configPath =
  addPath "_config"

-- | Base path for all section requests
sectionPath :: DocId -- ^ Section name
            -> RequestBuilder ()
sectionPath s = do
  configPath
  selectDoc s

-- | Base path for all item requests
itemPath :: DocId -- ^ Section name
         -> DocId -- ^ Key name
         -> RequestBuilder ()
itemPath s k = do
  sectionPath s
  selectDoc k
