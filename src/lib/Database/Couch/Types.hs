{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, OverloadedStrings #-}

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
import Text.Show (
  Show,
  )

-- | The name of the database to connect to
newtype Db = Db Text deriving (Eq, IsString, Show)

-- | The id of the document to work on
newtype DocId = DocId Text deriving (Eq, IsString, Show)

-- | The name of the host to connect to
newtype Host = Host Text deriving (Eq, IsString, Show)

-- | The password for the user you are connecting as
newtype Password = Password Text deriving (Eq, IsString, Show)

-- | The number of the port to connect to
newtype Port = Port Int deriving (Eq, Show)

-- | The name of the user to connect as
newtype User = User Text deriving (Eq, IsString, Show)
