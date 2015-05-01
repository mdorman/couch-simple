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
import           Data.Aeson                    (Value)
import           Data.Either                   (Either)
import           Data.Maybe                    (Maybe)
import           Data.Text                     (Text)
import           Database.Couch.Internal       (structureRequest, valueRequest)
import           Database.Couch.RequestBuilder (RequestBuilder, addPath,
                                                selectDoc, setJsonBody,
                                                setMethod)
import           Database.Couch.ResponseParser (checkStatusCode, responseValue)
import           Database.Couch.Types          (Context, CouchError, DocId)
import           Network.HTTP.Client           (CookieJar)

-- | Get the configuration for the overall server.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config API documentation>
--
-- Returns a (structured) JSON Value.
--
-- Status: __Complete__
server :: MonadIO m => Context -> m (Either CouchError (Value, Maybe CookieJar))
server =
  structureRequest request parse
  where
    request =
      addPath "_config"
    parse = do
      checkStatusCode
      responseValue

-- | Get the configuration for one section.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns a (structured) JSON Value.
--
-- Status: __Complete__
section :: MonadIO m => DocId -> Context -> m (Either CouchError (Value, Maybe CookieJar))
section s =
  structureRequest request parse
  where
    request = do
      addPath "_config"
      selectDoc s
    parse = do
      checkStatusCode
      responseValue

-- | Get the configuration for one item.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns the JSON Value.
--
-- Status: __Complete__
getValue :: MonadIO m => DocId -> DocId -> Context -> m (Either CouchError (Value, Maybe CookieJar))
getValue s k =
  valueRequest request parse
  where
    request =
      configPath s k
    parse = do
      checkStatusCode
      responseValue

-- | Set the configuration value for one item.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns the previous JSON Value.
--
-- Status: __Complete__
setValue :: MonadIO m => DocId -> DocId -> Text -> Context -> m (Either CouchError (Value, Maybe CookieJar))
setValue s k v =
  valueRequest request parse
  where
    request = do
      setMethod "PUT"
      configPath s k
      setJsonBody v
    parse = do
      checkStatusCode
      responseValue

-- | Remove the configuration value for one item.
--
-- <http://docs.couchdb.org/en/1.6.1/api/server/configuration.html#get--_config-section API documentation>
--
-- Returns the previous JSON Value.
--
-- Status: __Complete__
delValue :: MonadIO m => DocId -> DocId -> Context -> m (Either CouchError (Value, Maybe CookieJar))
delValue s k =
  valueRequest request parse
  where
    request = do
      setMethod "DELETE"
      configPath s k
    parse = do
      checkStatusCode
      responseValue

configPath :: DocId -> DocId -> RequestBuilder ()
configPath s k = do
  addPath "_config"
  selectDoc s
  selectDoc k
