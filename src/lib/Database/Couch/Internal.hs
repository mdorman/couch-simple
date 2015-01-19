{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude #-}

{- |

Module      : Database.Couch.Internal
Description : The lowest low-level code for Database.Couch
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This module is about things that are at such a low level, they're not
even necessarily really CouchDB-specific.

-}

module Database.Couch.Internal where

import Control.Monad (
  return,
  )
import Control.Monad.Catch (
  Exception,
  MonadThrow,
  SomeException,
  throwM,
  try,
  )
import Control.Monad.IO.Class (
  MonadIO,
  liftIO,
  )
import Data.Aeson (
  Value,
  json,
  )
import Data.Attoparsec.ByteString (
  IResult (Done, Fail, Partial),
  parseWith,
  )
import Data.Either (
  Either,
  )
import Data.Function (
  ($),
  )
import Data.Text (
  Text,
  pack,
  )
import Data.Typeable (
  Typeable
  )
import Network.HTTP.Client (
  Manager,
  Request,
  brRead,
  responseBody,
  responseHeaders,
  responseStatus,
  withResponse,
  )
import Network.HTTP.Types (
  ResponseHeaders,
  Status,
  )
import Text.Show (
  Show
  )

{- |

Failure modes for making CouchDB requests.  These will come to cover
the gamut from failure to parse a particular JSON value to document
conflicts.

-}

data CouchException
  -- | We ran out of input before we succeeded in parsing a JSON
  -- 'Data.Aeson.Value'.
  = ParseIncomplete
  -- | There was some sort of syntactic issue with the text we were
  -- attempting to parse.
  | ParseFail Text
  deriving (Show, Typeable)
instance Exception CouchException

{- |

This is our lowest-level non-streaming routine.  It only handles
performing the request and parsing the result into a JSON value.

It presumes:

 * we will be receiving a deserializable JSON structure

 * we do not need to stream out the result (though the input is parsed
incrementally)

In the interest of giving the most obvious interface, this routine
will catch any exceptions that might be thrown by lower level and
return them as a 'Data.Either.Left' value.

Basing the rest of our library on a function where all dependencies
are explicit should help make sure that other bits remain portable to,
say, streaming interfaces.

-}

jsonRequest :: (MonadIO m, MonadThrow m) => Manager -> Request -> m (Either SomeException (ResponseHeaders, Status, Value))
jsonRequest manager request = do
  liftIO $ try $ withResponse request manager streamParse
  where
    streamParse res = do
      let input = brRead (responseBody res)
      initial <- input
      result <- parseWith input json initial
      -- Using throw here piggy-backs on the fact that the http-client
      -- routines will throw exceptions.
      case result of
        (Done _ ret) -> return (responseHeaders res, responseStatus res, ret)
        (Partial _) -> throwM ParseIncomplete
        (Fail _ _ err) -> throwM $ ParseFail $ pack err
