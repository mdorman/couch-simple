{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}

{- |

Module      : Database.Couch.Types
Description : Types for managing access to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

Types for working with a CouchDB database.

-}

module Database.Couch.Types where

import Data.Aeson (
  FromJSON,
  ToJSON,
  )
import Data.ByteString (
  ByteString,
  )
import Data.Eq (
  Eq,
  )
import Data.Function (
  (.),
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
import Data.String (
  IsString,
  )
import Data.Text (
  Text,
  )
import Data.Text.Encoding (
  encodeUtf8,
  )
import Network.HTTP.Client (
  CookieJar,
  HttpException,
  Manager,
  )
import Text.Show (
  Show,
  )

{- | Failure modes for making CouchDB requests.

These will come to cover the gamut from failure to parse a particular
JSON value to document conflicts.  The return values of our routines
will consistently be ('Either' 'CouchError', a).

-}

data CouchError
  -- | The database already exists
  = AlreadyExists
  -- | The server complained about the content of our request.  Sounds
  -- like the library is broken. :(
  | HttpError HttpException
  -- | The server complained about the content of our request.  Sounds
  -- like the library is broken. :(
  | ImplementationError Text
  -- | The name you tried to give for the DB is invalid
  | InvalidName Text
  -- | The thing you were looking for was not found
  | NotFound
  -- | We ran out of input before we succeeded in parsing a JSON
  -- 'Data.Aeson.Value'.
  | ParseIncomplete
  -- | There was some sort of syntactic issue with the text we were
  -- attempting to parse.
  | ParseFail Text
  -- | The credentials you used do not have access to this resource
  | Unauthorized
  -- | Don't understand the failure
  | Unknown
  deriving (Show)

-- | The name of the database to connect to
newtype Db = Db { unwrapDb :: Text } deriving (Eq, FromJSON, IsString, Show, ToJSON)

-- | The id of the document to work on
newtype DocId = DocId { unwrapDocId :: Text } deriving (Eq, FromJSON, IsString, Show, ToJSON)

-- | Convert a DocId directly into a 'ByteString'
reqDocId :: DocId -> ByteString
reqDocId = encodeUtf8 . unwrapDocId

-- | The revision of the document to work on
newtype DocRev = DocRev { unwrapDocRev :: Text } deriving (Eq, FromJSON, IsString, Show, ToJSON)

-- | Convert a DocRev directly into a 'ByteString'
reqDocRev :: DocRev -> ByteString
reqDocRev = encodeUtf8 . unwrapDocRev

-- | The name of the host to connect to
newtype Host = Host { unwrapHost :: Text } deriving (Eq, FromJSON, IsString, Show, ToJSON)

-- | The password for the user you are connecting as
newtype Password = Password { unwrapPassword :: Text } deriving (Eq, FromJSON, IsString, Show, ToJSON)

basicPass :: Password -> ByteString
basicPass = encodeUtf8 . unwrapPassword

-- | The number of the port to connect to
newtype Port = Port { unwrapPort :: Int } deriving (Eq, Show)

-- | The name of the user to connect as
newtype User = User { unwrapUser ::  Text } deriving (Eq, FromJSON, IsString, Show, ToJSON)

basicUser :: User -> ByteString
basicUser = encodeUtf8 . unwrapUser

{- | The context for each CouchDB request.

This contains all the bits that are unlikely to vary between requests.

Eventually, we should have routines that are smart enough to pull this
out of a suitably-set-up Monad, so you could just stash it there and
forget about it.

-}

data Context
 = Context {
   -- | The Manager that "Network.HTTP.Client" requests require.  We
   -- store it here for easy access.
   ctxManager :: Manager,
   -- | The host to connect to
   ctxHost    :: Host,
   -- | The port to connect to
   ctxPort    :: Port,
   -- | Any credentials that should be used in making requests
   ctxCred    :: Maybe Credentials,
   -- | We can trade credentials for a session cookie that is more
   -- efficient, this is where it can be stored.
   ctxCookies :: CookieJar,
   -- | The database that should be used for database-specific
   -- requests.
   ctxDb      :: Maybe Db
   }

-- | Pull the appropriately encoded database out of the context
reqDb :: Context -> ByteString
reqDb c = maybe mempty (encodeUtf8 . unwrapDb) (ctxDb c)

-- | Pull the appropriately encoded host out of the context
reqHost :: Context -> ByteString
reqHost = encodeUtf8 . unwrapHost . ctxHost

-- | Pull the appropriately encoded port out of the context
reqPort :: Context -> Int
reqPort = unwrapPort . ctxPort

{- | The credentials for each CouchDB request.

Many operations in CouchDB require some sort of authentication.  We
will store the credentials in their various forms here (though we're
sticking to HTTP Basic Authentication for now).

There are operations on the request that know how to modify the
request appropriately depending on which credential type is in play.

-}

data Credentials
  = Basic {
    credUser :: User,
    credPass :: Password
    }

-- | A quick type alias for query parameters.
type QueryParameters = [(ByteString, Maybe ByteString)]

-- | A typeclass for types that can be converted to query parameters.
class ToQueryParameters a where
  -- | Performs the actual conversion
  toQueryParameters :: a -> QueryParameters

-- | Result type for creating a new document in a database.
data CreateResult
  -- | In batch mode, you don't get a rev back
  = NoRev DocId
  -- | Otherwise, you do get the rev back for your doc
  | WithRev DocId DocRev
