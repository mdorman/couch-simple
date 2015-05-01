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

import           Control.Monad                 (return, (>>=))
import           Control.Monad.Catch           (handle)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson                    (Value (Null))
import           Data.Aeson.Parser             (json, value)
import           Data.Attoparsec.ByteString    (IResult (Done, Fail, Partial),
                                                Parser, parseWith)
import           Data.Either                   (Either (Right, Left), either)
import           Data.Eq                       ((==))
import           Data.Function                 (const, ($), (.))
import           Data.Maybe                    (Maybe (Just, Nothing))
import           Data.Monoid                   (mempty)
import           Data.Text                     (pack)
import           Database.Couch.RequestBuilder (RequestBuilder, runBuilder)
import           Database.Couch.ResponseParser (ResponseParser, runParse)
import           Database.Couch.Types          (Context, CouchError (HttpError, ParseFail, ParseIncomplete),
                                                ctxCookies, ctxManager)
import           Network.HTTP.Client           (CookieJar, Manager, Request,
                                                brRead, checkStatus, method,
                                                responseBody, responseCookieJar,
                                                responseHeaders, responseStatus,
                                                withResponse)
import           Network.HTTP.Types            (ResponseHeaders, Status,
                                                methodHead)

{- | Make an HTTP request returning a JSON value

This is our lowest-level non-streaming routine.  It only handles
performing the request and parsing the result into a JSON value.

It presumes:

 * we will be receiving a deserializable JSON value

 * we do not need to stream out the result (though the input is parsed
incrementally)

The results of parsing the stream will be handed to a routine that
take the output and return the value the user ultimately desires.  We
use "Data.Either" to handle indicating failure and such.

Basing the rest of our library on a function where all dependencies
are explicit should help make sure that other bits remain portable to,
say, streaming interfaces.

-}

rawJsonRequest :: MonadIO m => Parser Value -> Manager -> Request -> m (Either CouchError (ResponseHeaders, Status, CookieJar, Value))
rawJsonRequest parser manager request =
  liftIO (handle errorHandler $ withResponse request { checkStatus = const . const . const Nothing } manager responseHandler)
  where
    -- Simply convert any exception into an HttpError
    errorHandler =
       return . Left . HttpError
    -- Incrementally parse the body, reporting failures.
    responseHandler res = do
      result <- if method request == methodHead
                then return (Done mempty Null)
                else parseParts res
      return $ case result of
        (Done _ ret) -> return (responseHeaders res, responseStatus res, responseCookieJar res, ret)
        (Partial _) -> Left ParseIncomplete
        (Fail _ _ err) -> Left $ ParseFail $ pack err
    parseParts res = do
      let input = brRead (responseBody res)
      initial <- input
      parseWith input parser initial

mkParsedRequest :: MonadIO m => Parser Value -> RequestBuilder () -> ResponseParser a -> Context -> m (Either CouchError (a, Maybe CookieJar))
mkParsedRequest jsonParser builder parse context =
  parsedRequest jsonParser manager request >>= parser
  where
    manager =
      ctxManager context
    request =
      runBuilder builder context
    parser =
      return . either Left parseContext
    parseContext (h, s, c, v) =
      runParse parse (Right (h, s, v)) >>= checkContextUpdate c
    checkContextUpdate c a =
      Right (a, if c == ctxCookies context then Nothing else Just c)


{- | Define and make an HTTP request returning a JSON structure

Building on top of 'mkParsedRequest', this routine is designed to take
a builder for the request and a parser for the result, and use them to
make our transaction.  This makes for a very declarative style when
defining individual endpoints for CouchDB.

In order to support more sophisticated forms of authentication than
'Basic', we do have to examine the cookie jar returned from the
server, and perhaps tell the user that they should replace the cookie
jar in their context with it.

-}

makeJsonRequest :: MonadIO m => RequestBuilder () -> ResponseParser a -> Context -> m (Either CouchError (a, Maybe CookieJar))
makeJsonRequest =
  mkParsedRequest json

{- | Define and make an HTTP request returning a JSON value

This works identically to 'makeJsonRequest', except it is more liberal
in the values that it will parse.

-}

makeValueRequest :: MonadIO m => RequestBuilder () -> ResponseParser a -> Context -> m (Either CouchError (a, Maybe CookieJar))
makeValueRequest =
  mkParsedRequest value
