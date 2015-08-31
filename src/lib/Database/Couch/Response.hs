{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

{- |

Module      : Database.Couch.Response
Description : Utilities for extracting specific types from Database.Couch JSON values
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

Calls to CouchDB can return values that have well-defined structure beyond their simple JSON content, but that don't necessarily warrant full-blown data types with 'FromJSON' instances and the like.  We want to provide convenient conversions when that is the case.

> result <- Database.compact cxt `ap` asBool
> if result
>   then ...
>   else ...

-}

module Database.Couch.Response where

import           Control.Monad        ((>>=))
import           Data.Aeson           (FromJSON, Value (Object), fromJSON)
import qualified Data.Aeson           as Aeson (Result (Error, Success))
import           Data.Bool            (Bool)
import           Data.Either          (Either (Left, Right))
import           Data.Function        (($), (.))
import           Data.Functor         (fmap)
import           Data.HashMap.Strict  (lookup)
import           Data.Maybe           (Maybe (Just, Nothing), catMaybes, maybe)
import           Data.String          (fromString)
import           Data.Text            (Text, intercalate, splitAt)
import           Data.Text.Encoding   (encodeUtf8)
import           Data.UUID            (UUID, fromASCIIBytes)
import           Database.Couch.Types (Error (NotFound, ParseFail), Result)

{- | Attempt to decode the value into anything with a FromJSON constraint.

This is really about translating 'Data.Aeson.Result' values into our 'Database.Couch.Types.Result' values. -}

asAnything :: FromJSON a => Result Value -> Result a
asAnything v =
  case v of
    Left x             -> Left x
    Right (a, b) -> case fromJSON a of
      Aeson.Error e   -> (Left . ParseFail . fromString) e
      Aeson.Success s -> Right (s, b)

{- | Attempt to construct a 'Data.Bool.Bool' value.

This assumes the routine conforms to CouchDB's @{"ok": true}@ return convention. -}
asBool :: Result Value -> Result Bool
asBool = getKey "ok"

{- | Attempt to construct a list of 'Data.UUID.UUID' values.

CouchDB returns uuids as string values in a form that "Data.UUID" cannot consume directly, so we provide this standard conversion. -}
asUUID :: Result Value -> Result [UUID]
asUUID v =
  case v of
    Left x              -> Left x
    Right (Object o, b) -> maybe (Left (ParseFail "Couldn't convert to UUID type"))
                             (Right . (,b) . catMaybes . reformat) $ lookup "uuids" o
    _                   -> Left NotFound
  where
    reformat i =
      case fromJSON i of
        Aeson.Error _   -> []
        Aeson.Success a -> fmap (fromASCIIBytes . encodeUtf8 . reformatUuid) a
    reformatUuid s =
      let (first, second') = splitAt 8 s
          (second, third') = splitAt 4 second'
          (third, fourth') = splitAt 4 third'
          (fourth, fifth) = splitAt 4 fourth'
      in intercalate "-" [first, second, third, fourth, fifth]

{- | Attempt to extract the value of a particular key. -}
getKey :: FromJSON a => Text -> Result Value -> Result a
getKey k v  =
  case v of
    Left x              -> Left x
    Right (Object o, b) -> maybe (Left NotFound) (Right . (, b)) $ lookup k o >>= reformat
    _                   -> Left NotFound
  where
    reformat i =
      case fromJSON i of
      Aeson.Error _   -> Nothing
      Aeson.Success a -> Just a
