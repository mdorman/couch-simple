{- |

Module      : Database.Couch
Description : A modern, lightweight, complete client for CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

Database.Couch is intended to be a modern, lightweight, complete client for <http://couchdb.apache.org/ CouchDB>.  So far, it is modern and lightweight, but not yet complete.

All of the modules in this package are intended to be @import qualified@.  No attempt has been made to keep names of functions from clashing with obvious or otherwise commonly-used names---or even with other modules in the package.  Types are often unique, but again, no guarantees.

At the moment the only available interface is the /Explicit/ interface, where all aspects of the interface are directly exposed.

All types are located in "Database.Couch.Types".

The /Explicit/ interface consists of:

* "Database.Couch.Explicit.Server"

    Server functionality, like getting a list of databases or starting replication.

* "Database.Couch.Explicit.Configuration"

    Server configuration handling.

* "Database.Couch.Explicit.Database"

    Database functionality, like creation, or cleaning and compacting.

* "Database.Couch.Explicit.Design"

    Design document handling, like creation and querying.

* "Database.Couch.Explicit.Doc"

    Document handling.

* "Database.Couch.Explicit.Local"

    Local (unreplicated) document handling.

-}

module Database.Couch where
