{- |

Module      : Database.Couch
Description : An overview of the package
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

Database.Couch is intended to be a modern, lightweight, complete client for <http://couchdb.apache.org/ CouchDB>.  So far, it is modern and lightweight, but not yet complete.

All of the modules in this package are intended to be @import qualified@.  No attempt has been made to keep names of functions from clashing with obvious or otherwise commonly-used names---or even with other modules in the package.  Types are often unique, but again, no guarantees.

At the moment the only available interface is the "Database.Couch.Explicit" interface, where all aspects of the interface are directly exposed.  Please see that module for an overview.

All types are located in "Database.Couch.Types".

-}

module Database.Couch where
