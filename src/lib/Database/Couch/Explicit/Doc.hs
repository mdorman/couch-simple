{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Module      : Database.Couch.Explicit.Doc
Description : Document-oriented requests to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is intended to be @import qualified@.  /No attempt/ has
been made to keep names of types or functions from clashing with
obvious or otherwise commonly-used names.

The functions here are derived from (and presented in the same order
as) http://docs.couchdb.org/en/1.6.1/api/doc/index.html.

-}

module Database.Couch.Explicit.Doc where

import           Control.Monad.IO.Class        (MonadIO)
import           Data.Aeson                    (FromJSON, Value (Number),
                                                object)
import           Data.Either                   (Either)
import           Data.Function                 (($))
import           Data.Maybe                    (Maybe)
import           Database.Couch.Internal       (structureRequest)
import           Database.Couch.RequestBuilder (maybeAddRev, selectDb,
                                                selectDoc, setMethod,
                                                setQueryParam)
import           Database.Couch.ResponseParser (failed, getContentLength,
                                                getDocRev, responseStatus,
                                                toOutputType)
import           Database.Couch.Types          (Context,
                                                CouchError (NotFound, Unknown),
                                                DocGetDoc, DocId, DocRev,
                                                toQueryParameters, unwrapDocRev)
import           GHC.Num                       (fromInteger)
import           Network.HTTP.Client           (CookieJar)
import           Network.HTTP.Types            (statusCode)

-- | Get the size and revision of the specified document.
--
-- <http://docs.couchdb.org/en/1.6.1/api/document/common.html#head--db-docid API documentation>
--
-- Returns a tuple of (Int, DocRev).  Since we're extracting this from
-- headers, we don't worry with a JSON representation.
--
-- Status: __Broken__
size :: (FromJSON a, MonadIO m) => DocGetDoc -> DocId -> Maybe DocRev -> Context -> m (Either CouchError (a, Maybe CookieJar))
size param doc rev =
  structureRequest request parse
  where
    request = do
      setMethod "HEAD"
      selectDb
      selectDoc doc
      maybeAddRev rev
      setQueryParam $ toQueryParameters param
    parse = do
      -- Check status codes by hand because we don't want 404 to be an
      -- error, just False
      s <- responseStatus
      docRev <- getDocRev
      contentLength <- getContentLength
      case statusCode s of
        200 -> toOutputType $ object [(unwrapDocRev docRev, Number $ fromInteger contentLength)]
        404 -> failed NotFound
        _   -> failed Unknown
