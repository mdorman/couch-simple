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

import Data.Eq (
  Eq,
  )
import Data.Int (
  Int,
  )
import Data.String (
  IsString,
  )
import Data.Text (
  Text,
  )
import Network.HTTP.Client (
  HttpException,
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
  -- | Something failed at the HTTP level.
  = HttpError HttpException
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
  deriving (Show)

-- | The name of the database to connect to
newtype Db = Db { unwrapDb :: Text } deriving (Eq, IsString, Show)

-- | The id of the document to work on
newtype DocId = DocId { unwrapDocId :: Text } deriving (Eq, IsString, Show)

-- | The name of the host to connect to
newtype Host = Host { unwrapHost :: Text } deriving (Eq, IsString, Show)

-- | The password for the user you are connecting as
newtype Password = Password { unwrapPassword :: Text } deriving (Eq, IsString, Show)

-- | The number of the port to connect to
newtype Port = Port { unwrapPort :: Int } deriving (Eq, Show)

-- | The name of the user to connect as
newtype User = User { unwrapUser ::  Text } deriving (Eq, IsString, Show)
