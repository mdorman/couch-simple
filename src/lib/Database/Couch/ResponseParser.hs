{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.ResponseParser
Description : Code for parsing responses from Database.Couch.External
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

These relatively simple combinators can do simple extractions of data from the data returned by "Database.Couch.External" routines, as well as checking certain information about the actual response values.

-}

module Database.Couch.ResponseParser where

import           Control.Monad              (return, (>>=))
import           Control.Monad.Reader       (Reader, asks, runReader)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Aeson                 (FromJSON, Result (Error, Success),
                                             Value (Object), fromJSON)
import           Data.ByteString            (ByteString)
import           Data.Either                (Either (Left, Right), either)
import           Data.Eq                    ((==))
import           Data.Foldable              (find)
import           Data.Function              (($), (.))
import           Data.Functor               (fmap)
import           Data.HashMap.Strict        (lookup)
import           Data.Maybe                 (Maybe, maybe)
import           Data.Monoid                (mempty)
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (decodeUtf8)
import           Data.Text.Read             (decimal)
import           Data.Tuple                 (fst, snd)
import           Database.Couch.Types       (DocRev (DocRev), Error (AlreadyExists, Conflict, HttpError, ImplementationError, InvalidName, NotFound, ParseFail, Unauthorized))
import           GHC.Integer                (Integer)
import           Network.HTTP.Client        (HttpException (StatusCodeException))
import           Network.HTTP.Types         (HeaderName, ResponseHeaders,
                                             Status, statusCode)

-- * Our primary interface

-- | Check the status code for a successful value and tries to decode to the user's desired type if so
standardParse :: FromJSON a => ResponseParser a
standardParse = do
  checkStatusCode
  responseValue >>= toOutputType

-- * Lower-level interfaces

-- | A type synonym for the Monad we're operating in
type ResponseParser = ExceptT Error (Reader (ResponseHeaders, Status, Value))

-- | Run a given parser over an initial value
runParse :: ResponseParser a -> Either Error (ResponseHeaders, Status, Value) -> Either Error a
runParse p (Right v) = (runReader . runExceptT) p v
runParse _ (Left v) = Left v

-- | Extract the response status from the Monad
responseStatus :: ResponseParser Status
responseStatus =
  asks status
  where
    status (_, s, _) = s

-- | Extract the response headers from the Monad
responseHeaders :: ResponseParser ResponseHeaders
responseHeaders =
  asks headers
  where
    headers (h, _, _) = h

-- | Extract the response value from the Monad
responseValue :: ResponseParser Value
responseValue =
  asks value
  where
    value (_, _, v) = v

-- | Check the status code for the response
checkStatusCode :: ResponseParser ()
checkStatusCode = do
  h <- responseHeaders
  s <- responseStatus
  case statusCode s of
    200 -> return ()
    201 -> return ()
    202 -> return ()
    304 -> return ()
    400 -> do
      error <- getKey "reason" >>= toOutputType
      throwE $ InvalidName error
    401 -> throwE Unauthorized
    404 -> throwE NotFound
    409 -> throwE Conflict
    412 -> throwE AlreadyExists
    415 -> throwE $ ImplementationError "The server says we sent a bad content type, which shouldn't happen.  Please open an issue at https://github.com/mdorman/couch-simple/issues with a test case if possible."
    _   -> throwE $ HttpError (StatusCodeException s h mempty)

-- | Try to retrieve a header from the response
maybeGetHeader :: HeaderName -> ResponseParser (Maybe ByteString)
maybeGetHeader header = do
  h <- responseHeaders
  return $ fmap snd (find ((== header) . fst) h)

-- | Retrieve a header from the response, or return an error if it's not present
getHeader :: HeaderName -> ResponseParser ByteString
getHeader header =
  maybeGetHeader header >>= maybe (throwE NotFound) return

-- | Decode the Content-Length header from the response, or return an error if it's not present
getContentLength :: ResponseParser Integer
getContentLength = do
  h <- getHeader "Content-Length"
  either (throwE . ParseFail . pack) (return . fst) $ decimal (decodeUtf8 h)

-- | Get the document revision (ETag header), or return an error if it's not present
getDocRev :: ResponseParser DocRev
getDocRev = do
  h <- getHeader "ETag"
  return $ DocRev $ decodeUtf8 h

-- | Get the value of a particular key from the response value, or return an error if it's not found
getKey :: Text -> ResponseParser Value
getKey key = do
  v <- responseValue
  case v of
    Object o -> maybe (throwE NotFound) return $ lookup key o
    _        -> throwE NotFound

-- | Decode the response value to a particular type, or return an error if it can't be decoded
toOutputType :: (FromJSON a) => Value -> ResponseParser a
toOutputType v =
  case fromJSON v of
    Error e -> throwE $ ParseFail $ pack e
    Success a -> return a
