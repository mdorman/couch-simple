{-# LANGUAGE NoImplicitPrelude #-}

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
  (>>=),
  return,
  )
import Control.Monad.Catch (
  handle,
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
  Either (Left),
  )
import Data.Function (
  ($),
  (.),
  const,
  )
import Data.Maybe (
  Maybe (Nothing),
  )
import Data.Text (
  pack,
  )
import Database.Couch.Types (
  CouchError (HttpError, ParseFail, ParseIncomplete),
  )
import Network.HTTP.Client (
  Manager,
  Request,
  brRead,
  checkStatus,
  responseBody,
  responseHeaders,
  responseStatus,
  withResponse,
  )
import Network.HTTP.Types (
  ResponseHeaders,
  Status,
  )

{- |

This is our lowest-level non-streaming routine.  It only handles
performing the request and parsing the result into a JSON value.

It presumes:

 * we will be receiving a deserializable JSON structure

 * we do not need to stream out the result (though the input is parsed
incrementally)

The results of parsing the stream will be handed to a routine that
take the output and return the value the user ultimately desires.  We
use "Data.Either" to handle indicating failure and such.

Basing the rest of our library on a function where all dependencies
are explicit should help make sure that other bits remain portable to,
say, streaming interfaces.

-}

jsonRequest :: MonadIO m => Manager -> Request -> (Either CouchError (ResponseHeaders, Status, Value) -> m (Either CouchError a)) -> m (Either CouchError a)
jsonRequest manager request parser =
  liftIO (handle errorHandler $ withResponse request { checkStatus = const . const . const Nothing } manager streamParse) >>= parser
  where
    -- Simply convert any exception into an HttpError
    errorHandler =
       return . Left . HttpError
    -- Incrementally parse the body, reporting failures.
    streamParse res = do
      let input = brRead (responseBody res)
      initial <- input
      result <- parseWith input json initial
      return $ case result of
        (Done _ ret) -> return (responseHeaders res, responseStatus res, ret)
        (Partial _) -> Left ParseIncomplete
        (Fail _ _ err) -> Left $ ParseFail $ pack err
