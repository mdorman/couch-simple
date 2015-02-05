{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}

{- |

Module      : Database.Couch.ResponseParser
Description : Code for parsing responses from CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

These relatively simple combinators can do simple extractions of data
from a CouchDB response, as well as checking certain information about
the actual response values.

-}

module Database.Couch.ResponseParser where

import Control.Monad (
  (>>=),
  return,
  )
import Control.Monad.Reader (
  Reader,
  runReader,
  asks,
  )
import Control.Monad.Trans.Either (
  EitherT,
  hoistEither,
  runEitherT
  )
import Data.Aeson (
  FromJSON,
  Result (Error, Success),
  Value (Object),
  fromJSON,
  )
import Data.ByteString (
  ByteString,
  )
import Data.Either (
  Either (Left, Right),
  either,
  )
import Data.Eq (
  (==),
  )
import Data.Foldable (
  find,
  )
import Data.Function (
  ($),
  (.),
  )
import Data.Functor (
  fmap,
  )
import Data.HashMap.Strict (
  lookup,
  )
import Data.Int (
  Int,
  )
import Data.Maybe (
  Maybe,
  maybe,
  )
import Data.Monoid (
  mempty,
  )
import Data.Text (
  Text,
  pack,
  )
import Data.Text.Encoding (
  decodeUtf8,
  )
import Data.Text.Read (
  decimal,
  )
import Data.Tuple (
  fst,
  snd,
  )
import Database.Couch.Types (
  CouchError (AlreadyExists, HttpError, ImplementationError, InvalidName, NotFound, ParseFail, Unauthorized),
  )
import Network.HTTP.Client (
  HttpException (StatusCodeException),
  )
import Network.HTTP.Types (
  HeaderName,
  ResponseHeaders,
  Status,
  statusCode,
  )

-- Just so we don't have to type this out Every. Damned. Time.
type ResponseParser = EitherT CouchError (Reader (ResponseHeaders, Status, Value))

-- Run a given parser over an initial value
runParse :: ResponseParser a -> Either CouchError (ResponseHeaders, Status, Value) -> Either CouchError a
runParse p (Right v) = (runReader . runEitherT) p v
runParse _ (Left v) = Left v

failed :: CouchError -> ResponseParser a
failed = hoistEither . Left

responseStatus :: ResponseParser Status
responseStatus =
  asks status
  where
    status (_, s, _) = s

responseHeaders :: ResponseParser ResponseHeaders
responseHeaders =
  asks headers
  where
    headers (h, _, _) = h

responseValue :: ResponseParser Value
responseValue =
  asks value
  where
    value (_, _, v) = v

checkStatusCode :: ResponseParser ()
checkStatusCode = do
  h <- responseHeaders
  s <- responseStatus
  case statusCode s of
    200 -> return ()
    201 -> return ()
    202 -> return ()
    400 -> do
      error <- getKey "reason" >>= toOutputType
      failed $ InvalidName error
    401 -> failed Unauthorized
    404 -> failed NotFound
    412 -> failed $ AlreadyExists
    415 -> failed $ ImplementationError "The server says we sent a bad content type, which shouldn't happen.  Please open an issue at https://github.com/mdorman/couch-simple/issues with a test case if possible."
    _   -> failed $ HttpError (StatusCodeException s h mempty)

maybeGetHeader :: HeaderName -> ResponseParser (Maybe ByteString)
maybeGetHeader header = do
  h <- responseHeaders
  return $ fmap snd (find ((== header) . fst) h)

getHeader :: HeaderName -> ResponseParser ByteString
getHeader header =
  maybeGetHeader header >>= maybe (failed NotFound) return

getContentLength :: ResponseParser Int
getContentLength = do
  h <- getHeader "Content-Length"
  either (failed . ParseFail . pack) (return . fst) $ decimal (decodeUtf8 h)

getKey :: Text -> ResponseParser Value
getKey key = do
  v <- responseValue
  case v of
    Object o -> maybe (failed NotFound) return $ lookup key o
    _        -> failed NotFound

toOutputType :: (FromJSON a) => Value -> ResponseParser a
toOutputType v =
  case (fromJSON v) of
    Error e -> failed $ ParseFail $ pack e
    Success a -> return a
