{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

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

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (mapM, mzero, return)
import           Data.Aeson              (FromJSON, ToJSON,
                                          Value (Array, Object, String),
                                          parseJSON, toJSON)
import           Data.Biapplicative      ((<<*>>))
import           Data.Bool               (Bool)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (intDec, toLazyByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Eq                 (Eq)
import           Data.Function           (($), (.))
import           Data.Functor            (fmap)
import qualified Data.HashMap.Strict     as HashMap (fromList, toList)
import           Data.Int                (Int)
import           Data.List               ((++))
import           Data.Maybe              (Maybe (Just, Nothing), catMaybes,
                                          maybe)
import           Data.Monoid             (mempty)
import           Data.String             (IsString)
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Vector             as Vector (fromList)
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (CookieJar, HttpException, Manager)
import           Text.Show               (Show)

{- | Failure modes for making CouchDB requests.

These will come to cover the gamut from failure to parse a particular
JSON value to document conflicts.  The return values of our routines
will consistently be ('Either' 'CouchError', a).

-}

data CouchError
  -- | The database already exists
  = AlreadyExists
  -- | The document already exists, and without the appropriate rev
  | Conflict
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

-- | Result type for creating a new document in a database.
data CreateResult
  -- | In batch mode, you don't get a rev back
  = NoRev DocId
  -- | Otherwise, you do get the rev back for your doc
  | WithRev DocId DocRev

-- | A quick type alias for query parameters.
type QueryParameters = [(ByteString, Maybe ByteString)]

-- | A typeclass for types that can be converted to query parameters.
class ToQueryParameters a where
  -- | Performs the actual conversion
  toQueryParameters :: a -> QueryParameters

-- | Helpers for converting values to Query Parameters
toQP :: ByteString -> (a -> ByteString) -> Maybe a -> Maybe (ByteString, Maybe ByteString)
toQP name fun = fmap ((name,) . Just . fun)

boolToQP :: ByteString -> Maybe Bool -> Maybe (ByteString, Maybe ByteString)
boolToQP name = toQP name (\bool -> if bool then "true" else "false")

docIdToQP :: ByteString -> Maybe DocId -> Maybe (ByteString, Maybe ByteString)
docIdToQP name = toQP name reqDocId

docRevToQP :: ByteString -> Maybe DocRev -> Maybe (ByteString, Maybe ByteString)
docRevToQP name = toQP name reqDocRev

intToQP :: ByteString -> Maybe Int -> Maybe (ByteString, Maybe ByteString)
intToQP name = toQP name (toStrict . toLazyByteString . intDec)

textToQP :: ByteString -> Maybe Text -> Maybe (ByteString, Maybe ByteString)
textToQP name = toQP name encodeUtf8

-- | Parameters for 'allDocs'.
data DbAllDocs
  = DbAllDocs {
    adConflicts     :: Maybe Bool,
    adDescending    :: Maybe Bool,
    adEndKey        :: Maybe Text,
    adEndKeyDocId   :: Maybe DocId,
    adIncludeDocs   :: Maybe Bool,
    adInclusiveEnd  :: Maybe Bool,
    adKey           :: Maybe Text,
    adLimit         :: Maybe Int,
    adSkip          :: Maybe Int,
    adStale         :: Maybe Bool,
    adStartKey      :: Maybe Text,
    adStartKeyDocId :: Maybe DocId,
    adUpdateSeq     :: Maybe Bool
    }
instance ToQueryParameters DbAllDocs where
  toQueryParameters DbAllDocs {..} = catMaybes [
    boolToQP "conflicts" adConflicts,
    boolToQP "descending" adDescending,
    textToQP "end_key" adEndKey,
    docIdToQP "end_key_doc_id" adEndKeyDocId,
    boolToQP "include_docs" adIncludeDocs,
    boolToQP "inclusive_end" adInclusiveEnd,
    textToQP "key" adKey,
    intToQP "limit" adLimit,
    intToQP "skip" adSkip,
    boolToQP "stale" adStale,
    textToQP "start_key" adStartKey,
    docIdToQP "start_key_doc_id" adStartKeyDocId,
    boolToQP "update_seq" adUpdateSeq
    ]

-- | The default (empty) parameters
dbAllDocs :: DbAllDocs
dbAllDocs = DbAllDocs Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Parameters for 'bulkDocs'.
data DbBulkDocs
  = DbBulkDocs {
    bdAllOrNothing :: Maybe Bool,
    bdFullCommit   :: Maybe Bool,
    bdNewEdits     :: Maybe Bool
    }

-- | The default (empty) parameters
dbBulkDocsParam :: DbBulkDocs
dbBulkDocsParam = DbBulkDocs Nothing Nothing Nothing

-- | Parameters for 'changes'.
data DbChanges
  = DbChanges {
    cDocIds          :: Maybe [DocId],
    cConflicts       :: Maybe Bool,
    cDescending      :: Maybe Bool,
    cFeed            :: Maybe FeedType,
    cFilter          :: Maybe Text,
    cHeartBeat       :: Maybe Int,
    cIncludeDocs     :: Maybe Bool,
    cAttachments     :: Maybe Bool,
    cAttEncodingInfo :: Maybe Bool,
    cLastEvent       :: Maybe Text,
    cSince           :: Maybe SinceType,
    cStyle           :: Maybe StyleType,
    cTimeout         :: Maybe Int,
    cView            :: Maybe Text
    }
instance ToQueryParameters DbChanges where
  toQueryParameters DbChanges {..} = catMaybes [
    boolToQP "conflicts" cConflicts,
    boolToQP "descending" cDescending,
    feedTypeToQP cFeed,
    textToQP "filter" cFilter,
    intToQP "heartbeat" cHeartBeat,
    boolToQP "include_docs" cIncludeDocs,
    boolToQP "attachments" cAttachments,
    boolToQP "att_encoding_info" cAttEncodingInfo,
    sinceTypeToQP cSince,
    styleTypeToQP cStyle,
    intToQP "timeout" cTimeout,
    textToQP "view" cView
    ]

-- | The default (empty) parameters
dbChangesParam :: DbChanges
dbChangesParam = DbChanges Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | Parameters for 'dbUpdates'.
data DbUpdates
  = DbUpdates {
    feed      :: Maybe FeedType,
    timeOut   :: Maybe Int,
    heartBeat :: Maybe Bool
    }
instance ToQueryParameters DbUpdates where
  toQueryParameters DbUpdates {..} = catMaybes [
    feedTypeToQP feed,
    intToQP "timeout" timeOut,
    boolToQP "heartbeat" heartBeat
    ]

-- | The default (empty) parameters
dbUpdatesParam :: DbUpdates
dbUpdatesParam = DbUpdates Nothing Nothing Nothing

-- | Parameters for 'getDoc'.
data DocGetDoc
  = DocGetDoc {
    dgdAttachments      :: Maybe Bool,
    dgdAttEncodingInfo  :: Maybe Bool,
    dgdAttsSince        :: [DocRev],
    dgdConflicts        :: Maybe Bool,
    dgdDeletedConflicts :: Maybe Bool,
    dgdLatest           :: Maybe Bool,
    dgdLocalSeq         :: Maybe Bool,
    dgdMeta             :: Maybe Bool,
    dgdOpenRevs         :: [DocRev],
    dgdRev              :: Maybe DocId,
    dgdRevs             :: Maybe Bool,
    dgdRevsInfo         :: Maybe Bool
    }
instance ToQueryParameters DocGetDoc where
  toQueryParameters DocGetDoc {..} = catMaybes $ [
    boolToQP "attachments" dgdAttachments,
    boolToQP "att_encoding_info" dgdAttEncodingInfo
    ] ++
    fmap (docRevToQP "atts_since" . Just) dgdAttsSince
--    boolToQP "atts_since" dgdAttsSince,
    ++ [
    boolToQP "conflicts" dgdConflicts,
    boolToQP "deleted_conflicts" dgdDeletedConflicts,
    boolToQP "latest" dgdLatest,
    boolToQP "local_seq" dgdLocalSeq,
    boolToQP "meta" dgdMeta
    ] ++
    fmap (docRevToQP "open_revs" . Just) dgdOpenRevs
    ++ [
    docIdToQP "rev" dgdRev,
    boolToQP "revs" dgdRevs,
    boolToQP "revs_info" dgdRevsInfo
    ]

-- | The default (empty) parameters
docGetDoc :: DocGetDoc
docGetDoc = DocGetDoc Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing

-- | Types of feeds available.
data FeedType
  = Continuous
  | EventSource
  | Longpoll

feedTypeToQP :: Maybe FeedType -> Maybe (ByteString, Maybe ByteString)
feedTypeToQP = fmap (("feed",) . Just . go)
    where
      go Continuous = "continuous"
      go EventSource = "eventsource"
      go Longpoll = "longpoll"

-- | Possible values of since
data SinceType
  = Now
  | Since Int

sinceTypeToQP :: Maybe SinceType -> Maybe (ByteString, Maybe ByteString)
sinceTypeToQP = fmap (("since",) . Just . go)
    where
      go Now = "now"
      go (Since i) = (toStrict . toLazyByteString . intDec) i

-- | Possible values for style
data StyleType
  = StyleAll
  | StyleMain

styleTypeToQP :: Maybe StyleType -> Maybe (ByteString, Maybe ByteString)
styleTypeToQP = fmap (("style",) . Just . go)
    where
      go StyleAll = "all_docs"
      go StyleMain = "main_docs"

data DocRevMap
  = DocRevMap [(DocId, [DocRev])]
  deriving (Generic, Eq, Show)

instance FromJSON DocRevMap where
  parseJSON (Object o) = DocRevMap <$> mapM (\(k, v) -> (,) <$> (return . DocId $ k) <*> parseJSON v) (HashMap.toList o)
  parseJSON _ = mzero
instance ToJSON DocRevMap where
  -- The lack of symmetry in the outer and inner conversions annoys me, but I don't see how to make the outer point-free
  toJSON (DocRevMap d) = Object . HashMap.fromList $ fmap ((unwrapDocId, Array . Vector.fromList . fmap (String . unwrapDocRev)) <<*>>) d
