{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, OverloadedStrings #-}

{- |

Module      : Database.Couch.RequestBuilder
Description : Routines for creating the Request to send to CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

-}

module Database.Couch.RequestBuilder where

import Control.Monad (
  (>>),
  return,
  )
import Control.Monad.Reader (
  Reader,
  ask,
  runReader,
  )
import Control.Monad.State (
  MonadState,
  StateT,
  execStateT,
  get,
  put,
  )
import Data.ByteString (
  ByteString,
  intercalate,
  null,
  )
import Data.Default (
  def
  )
import Data.Eq (
  (==),
  )
import Data.Function (
  ($),
  on,
  )
import Data.List (
  unionBy
  )
import Data.Maybe (
  Maybe (Just),
  maybe,
  )
import Data.Monoid (
  (<>),
  mempty,
  )
import Data.Tuple (
  fst,
  )
import Database.Couch.Types (
  Context,
  Credentials (Basic),
  DocRev,
  basicPass,
  basicUser,
  ctxCookies,
  ctxCred,
  reqDb,
  reqDocRev,
  reqHost,
  reqPort,
  )
import Network.HTTP.Client (
  Request,
  applyBasicAuth,
  cookieJar,
  host,
  method,
  path,
  port,
  requestHeaders,
  setQueryString,
  )
import Network.HTTP.Types (
  RequestHeaders,
  hAccept,
  hContentType,
  )

-- | We have several things to track
data BuilderState =
  BuilderState {
    -- | The request being modified
    bsRequest ::Request,
    -- | The request itself only stores the 'queryString', so we track
    -- these raw pairs during construction, and use them to set them
    -- at the end.
    bsQueryParam :: [(ByteString, Maybe ByteString)],
    -- | If this is set, it will be prepended to the path.
    bsDb :: ByteString,
    -- | Again, stored this way for ease of manipulation, then
    -- properly assembled at the end.
    bsPathSegments :: [ByteString]
    }

-- | Our initial 'BuilderState'
start :: BuilderState
start = BuilderState def [] mempty []

-- Just so we don't have to type this out Every. Damned. Time.
type RequestBuilder = StateT BuilderState (Reader Context)

-- | Given a 'Context', run our monadic builder function to produce a
-- 'Request'.
runBuilder :: RequestBuilder () -> Context -> Request
runBuilder builder context =
  finalize (runReader (execStateT (defaultRequest >> builder) start) context)

-- | This actually takes the 'BuilderState' and does the assembly of
-- the various state bits into a single 'Request'.
finalize :: BuilderState -> Request
finalize (BuilderState r q d p) =
  setQueryString q r { path = calculatedPath }
  where
    calculatedPath =  "/" <> intercalate "/" ((if null d then [] else [d]) <> p)

{- | The default set of modifications applied to the request.

* The host/port connection information is set

* The 'Accept' header is set to 'application/json'

* The 'Content-Type' headers is set to 'application/json'

* Any authentication session in the cookie jar is set

* Any authentication information is applied

Any or all of these may be overridden, but probably shouldn't be.

-}
defaultRequest :: RequestBuilder ()
defaultRequest = do
  defaultHeaders [(hAccept, "application/json"), (hContentType, "application/json")]
  setAuth
  setConnection
  setCookieJar
  setMethod "GET"

-- | Add headers to a 'Request', leaving existing instances
-- undisturbed.
addHeaders :: RequestHeaders -> RequestBuilder ()
addHeaders new = do
  (BuilderState r q d p) <- get
  let headers = requestHeaders r
  put $ BuilderState r { requestHeaders = headers <> new } q d p

-- | Add headers to a 'Request', if they aren't already present
defaultHeaders :: RequestHeaders -> RequestBuilder ()
defaultHeaders new = do
  (BuilderState r q d p) <- get
  let headers = requestHeaders r
  put $ BuilderState r { requestHeaders = unionBy ((==) `on` fst) headers new } q d p

-- | Set headers on the 'Request', removing any existing instances.
setHeaders :: RequestHeaders -> RequestBuilder ()
setHeaders new = do
  (BuilderState r q d p) <- get
  let headers = requestHeaders r
  put $ BuilderState r { requestHeaders = unionBy ((==) `on` fst) new headers } q d p

-- | Add query parameters to a 'Request', leaving existing parameters
-- undisturbed.
addQueryParam :: [(ByteString, Maybe ByteString)] -> RequestBuilder ()
addQueryParam new = do
  (BuilderState r q d p) <- get
  put $ BuilderState r (q <> new) d p

-- | Add query parameters to a 'Request', if they aren't already
-- present
defaultQueryParam :: [(ByteString, Maybe ByteString)] -> RequestBuilder ()
defaultQueryParam new = do
  (BuilderState r q d p) <- get
  put $ BuilderState r (unionBy ((==) `on` fst) q new) d p

-- | Set query parameters on the 'Request', removing any existing
-- instances.
setQueryParam :: [(ByteString, Maybe ByteString)] -> RequestBuilder ()
setQueryParam new = do
  (BuilderState r q d p) <- get
  put $ BuilderState r (unionBy ((==) `on` fst) new q) d p

-- | Choose the database for the 'Request', based on what's in the
-- 'Context'.  This is the one thing that could arguably throw an
-- error.
selectDb :: RequestBuilder ()
selectDb = do
  c <- ask
  (BuilderState r q _ p) <- get
  put $ BuilderState r q (reqDb c) p

-- | Set the appropriate authentication markers on the 'Request', based
-- on what's in the 'Context'
setAuth :: RequestBuilder ()
setAuth = do
  c <- ask
  maybe (return ()) doApply (ctxCred c)
  where
    doApply cred = do
      (BuilderState r q d p) <- get
      put $ BuilderState (applyCred cred r) q d p
    applyCred (Basic u p) = applyBasicAuth (basicUser u) (basicPass p)

-- | Set the host and port for the 'Request', based on what's in the
-- 'Context'
setConnection :: RequestBuilder ()
setConnection = do
  c <- ask
  (BuilderState r q d p) <- get
  put $ BuilderState r { host = reqHost c, port = reqPort c } q d p

-- | Set the 'CookieJar' for the 'Request', based on what's in the
-- 'Context'
setCookieJar :: RequestBuilder ()
setCookieJar = do
  c <- ask
  (BuilderState r q d p) <- get
  put $ BuilderState r { cookieJar = Just $ ctxCookies c } q d p

-- | Set the method for the 'Request'.
setMethod :: ByteString -> RequestBuilder ()
setMethod m = do
  (BuilderState r q d p) <- get
  put $ BuilderState r { method = m } q d p

-- | Set the path for the 'Request'.  This is only appropriate for
-- static paths.
addPath :: ByteString -> RequestBuilder ()
addPath new = do
  (BuilderState r q d p) <- get
  put $ BuilderState r q d (p <> [new])

-- | Set the rev for the 'Request'.
addRev :: DocRev -> RequestBuilder ()
addRev rev =
  setHeaders [("ETag", reqDocRev rev)]

-- | Set the rev for the 'Request' if you have it.
maybeAddRev :: Maybe DocRev -> RequestBuilder ()
maybeAddRev =
  maybe (return ()) addRev
