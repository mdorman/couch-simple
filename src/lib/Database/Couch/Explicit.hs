{- |

Module      : Database.Couch.Explicit
Description : An overview of the /Explicit/ interface
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

This is a mid-layer interface to <http://couchdb.apache.org/ CouchDB>.  All information necessary for each operation has to be provided directly for each function, and similarly all necessary data is returned explicitly.

All of the modules of the /Explicit/ interface are intended to be @import qualified@.  No attempt has been made to keep names of functions from clashing with obvious or otherwise commonly-used names---or even with other modules in this package.

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

module Database.Couch.Explicit where
