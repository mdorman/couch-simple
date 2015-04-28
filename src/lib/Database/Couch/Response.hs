{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

Module      : Database.Couch.Response
Description : Utilities for extracting specific types from Database.Couch JSON values
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

Calls to CouchDB can return values that sometimes have structure
beyond their simple JSON content.  We want to provide convenient
conversions when that is the case.

> result <- Database.compact cxt `ap` asBool
> if result
>   then ...
>   else ...

-}

module Database.Couch.Response where

import           Data.Aeson           (Result (Error, Success),
                                       Value (Array, Object), fromJSON)
import           Data.Bool            (Bool (True))
import           Data.Either          (Either (Left, Right))
import           Data.Function        (const, ($), (.))
import           Data.Functor         (fmap)
import           Data.HashMap.Strict  (lookup)
import           Data.Maybe           (Maybe, catMaybes, maybe)
import           Data.Text            (intercalate, splitAt)
import           Data.Text.Encoding   (encodeUtf8)
import           Data.UUID            (UUID, fromASCIIBytes)
import           Database.Couch.Types (CouchError (NotFound, ParseFail))
import           Network.HTTP.Client  (CookieJar)

{- | Attempt to construct a 'Data.Bool.Bool' value.

This assumes the routine conforms to CouchDB's @{"ok": true}@ return convention.

-}
asBool :: Either CouchError (Value, Maybe CookieJar) -> Either CouchError (Bool, Maybe CookieJar)
asBool v =
  case v of
    Left x              -> Left x
    Right (Object o, b) -> maybe (Left NotFound) (Right . const (True, b)) $ lookup "ok" o
    _                   -> Left NotFound

{- | Attempt to construct a list of 'Data.UUID.UUID' values.

CouchDB returns uuids as string values in a form that "Data.UUID"
cannot consume directly, so we provide this standard conversion.

-}
asUUID :: Either CouchError (Value, Maybe CookieJar) -> Either CouchError ([UUID], Maybe CookieJar)
asUUID v =
  case v of
    Left x                 -> Left x
    Right (a@(Array _), b) -> Right (catMaybes $ reformat a, b)
    _                      -> Left (ParseFail "Couldn't convert to UUID type")
  where
    reformat i =
      case fromJSON i of
        Error _   -> []
        Success a -> fmap (fromASCIIBytes . encodeUtf8 . reformatUuid) a
    reformatUuid s =
      let (first, second') = splitAt 8 s
          (second, third') = splitAt 4 second'
          (third, fourth') = splitAt 4 third'
          (fourth, fifth) = splitAt 4 fourth'
      in intercalate "-" [first, second, third, fourth, fifth]
