{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}

{- |

Module      : Database.Couch.Types
Description : Types for interacting with CouchDB
Copyright   : Copyright (c) 2015, Michael Alan Dorman
License     : MIT
Maintainer  : mdorman@jaunder.io
Stability   : experimental
Portability : POSIX

These types are intended for interacting with a CouchDB database.  We generally favor giving things distinct types for different uses, though this is not a hard and fast rule.

-}

module Database.Couch.Types where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (mapM, mzero, return)
import           Data.Aeson              (FromJSON, ToJSON,
                                          Value (Array, Object, String), object,
                                          parseJSON, toJSON, (.:), (.:?), (.=))
import           Data.Aeson.Types        (typeMismatch)
import           Data.Biapplicative      ((<<*>>))
import           Data.Bool               (Bool)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (intDec, toLazyByteString)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Either             (Either)
import           Data.Eq                 (Eq)
import           Data.Function           (($), (.))
import           Data.Functor            (fmap)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap (fromList, toList)
import           Data.Int                (Int)
import           Data.List               ((++))
import           Data.Maybe              (Maybe (Just, Nothing), catMaybes,
                                          maybe)
import           Data.Monoid             (mempty)
import           Data.String             (IsString)
import           Data.Text               (Text, null)
import           Data.Text.Encoding      (encodeUtf8)
import qualified Data.Vector             as Vector (fromList)
import           GHC.Generics            (Generic)
import           Network.HTTP.Client     (CookieJar, HttpException, Manager)
import           Network.HTTP.Types      (Header, HeaderName)
import           Text.Show               (Show)

{- | Failure modes for making CouchDB requests.

These will come to cover the gamut from failure to parse a particular JSON value to document conflicts.  We try to differentiate in useful ways without being slavish about it.

-}

data Error
  -- | The database already exists
  = AlreadyExists
  -- | The document already exists, and without the appropriate rev
  | Conflict
  -- | The server complained about the content of our request.  Sounds like the library is broken. :(
  | HttpError HttpException
  -- | The server complained about the content of our request.  Sounds like the library is broken. :(
  | ImplementationError Text
  -- | The name you tried to give for the DB is invalid
  | InvalidName Text
  -- | The thing you were looking for was not found
  | NotFound
  -- | We ran out of input before we succeeded in parsing a JSON 'Data.Aeson.Value'.
  | ParseIncomplete
  -- | There was some sort of syntactic issue with the text we were attempting to parse.
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

-- | Calls in the /Explicit/ interface will always return a 'Result'.
type Result a = Either Error (a, Maybe CookieJar)

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

-- | A typeclass for types that can be converted to headers.
class ToHTTPHeaders a where
  -- | Performs the actual conversion
  toHTTPHeaders :: a -> [Header]

-- | Helpers for converting values to HTTP Headers
toHH :: HeaderName -> (a -> ByteString) -> Maybe a -> Maybe Header
toHH name fun = fmap ((name,) . fun)

boolToHH :: HeaderName -> Maybe Bool -> Maybe Header
boolToHH name = toHH name (\bool -> if bool then "true" else "false")

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

-- | Parameters for 'Doc.put'
data DocPut
  = DocPut {
    dpFullCommit :: Maybe Bool,
    dpBatch      :: Maybe Bool
    }

instance ToHTTPHeaders DocPut where
  toHTTPHeaders DocPut {..} = catMaybes [
    boolToHH "X-Couch-Full-Commit" dpFullCommit
    ]

instance ToQueryParameters DocPut where
  toQueryParameters DocPut {..} = catMaybes [
    boolToQP "batch" dpBatch
    ]

-- | The default (empty) parameters
docPutParam :: DocPut
docPutParam = DocPut Nothing Nothing

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

data ViewSpec
  = ViewSpec {
    vsMap    :: Text,
    vsReduce :: Maybe Text
    } deriving (Generic, Eq, Show)

instance FromJSON ViewSpec where
  parseJSON (Object o) = ViewSpec <$> o .: "map" <*> o .:? "reduce"
  parseJSON v = typeMismatch "Couldn't extract ViewSpec: " v

instance ToJSON ViewSpec where
  toJSON ViewSpec {..} = object $ "map" .= vsMap : maybe [] (\v -> ["reduce" .= v]) vsReduce

data DesignDoc
  = DesignDoc {
    ddocId         :: DocId,
    ddocRev        :: DocRev,
    ddocLanguage   :: Maybe Text,
    ddocOptions    :: Maybe (HashMap Text Text),
    ddocFilters    :: Maybe (HashMap Text Text),
    ddocLists      :: Maybe (HashMap Text Text),
    ddocShows      :: Maybe (HashMap Text Text),
    ddocUpdates    :: Maybe (HashMap Text Text),
    ddocValidation :: Maybe Text,
    ddocViews      :: Maybe (HashMap Text ViewSpec)
    } deriving (Generic, Eq, Show)

instance FromJSON DesignDoc where
  parseJSON (Object o) = DesignDoc
                         <$> o .: "_id"
                         <*> o .: "_rev"
                         <*> o .:? "language"
                         <*> o .:? "options"
                         <*> o .:? "filters"
                         <*> o .:? "lists"
                         <*> o .:? "shows"
                         <*> o .:? "updates"
                         <*> o .:? "validate_doc_update"
                         <*> o .:? "views"
  parseJSON v = typeMismatch "Couldn't extract DesignDoc: " v

instance ToJSON DesignDoc where
  toJSON DesignDoc {..} = object $ catMaybes [
    if (null . unwrapDocId) ddocId
    then Nothing
    else Just ("_id" .= ddocId),
    if (null . unwrapDocRev) ddocRev
    then Nothing
    else Just ("_rev" .= ddocRev),
    fmap ("language" .=) ddocLanguage,
    fmap ("options" .=) ddocOptions,
    fmap ("filters" .=) ddocFilters,
    fmap ("lists" .=) ddocLists,
    fmap ("shows" .=) ddocShows,
    fmap ("updates" .=) ddocUpdates,
    fmap ("validate_doc_update" .=) ddocValidation,
    fmap ("views" .=) ddocViews
    ]

data ViewIndexInfo
  = ViewIndexInfo {
    viCompactRunning :: Bool,
    viDataSize       :: Int,
    viDiskSize       :: Int,
    viLanguage       :: Text,
    viPurgeSeq       :: Int,
    viSignature      :: Text,
    viUpdateSeq      :: Int,
    viUpdaterRunning :: Bool,
    viWaitingClients :: Int,
    viWaitingCommit  :: Bool
} deriving (Generic, Eq, Show)

instance FromJSON ViewIndexInfo where
  parseJSON (Object o) = ViewIndexInfo
                         <$> o .: "compact_running"
                         <*> o .: "data_size"
                         <*> o .: "disk_size"
                         <*> o .: "language"
                         <*> o .: "purge_seq"
                         <*> o .: "signature"
                         <*> o .: "update_seq"
                         <*> o .: "updater_running"
                         <*> o .: "waiting_clients"
                         <*> o .: "waiting_commit"
  parseJSON v = typeMismatch "Couldn't extract ViewIndexInfo: " v

instance ToJSON ViewIndexInfo where
  toJSON ViewIndexInfo {..} = object [
    "compact_running" .= viCompactRunning,
    "data_size" .= viDataSize,
    "disk_size" .= viDiskSize,
    "language" .= viLanguage,
    "purge_seq" .= viPurgeSeq,
    "signature" .= viSignature,
    "update_seq" .= viUpdateSeq,
    "updater_running" .= viUpdaterRunning,
    "waiting_clients" .= viWaitingClients,
    "waiting_commit" .= viWaitingCommit
    ]

-- | Parameters for view retrieval.
data ViewParams
  = ViewParams {
    vpAttachments     :: Maybe Bool,
    vpAttEncodingInfo :: Maybe Bool,
    vpConflicts       :: Maybe Bool,
    vpDescending      :: Maybe Bool,
    vpEndKey          :: Maybe Text,
    vpEndKeyDocId     :: Maybe DocId,
    vpGroup           :: Maybe Bool,
    vpGroupLevel      :: Maybe Int,
    vpIncludeDocs     :: Maybe Bool,
    vpInclusiveEnd    :: Maybe Bool,
    vpKey             :: Maybe Text,
    vpLimit           :: Maybe Int,
    vpReduce          :: Maybe Bool,
    vpSkip            :: Maybe Int,
    vpStale           :: Maybe Bool,
    vpStartKey        :: Maybe Text,
    vpStartKeyDocId   :: Maybe DocId,
    vpUpdateSeq       :: Maybe Bool
    }
instance ToQueryParameters ViewParams where
  toQueryParameters ViewParams {..} = catMaybes [
    boolToQP "attachments" vpAttachments,
    boolToQP "att_encoding_info" vpAttEncodingInfo,
    boolToQP "conflicts" vpConflicts,
    boolToQP "descending" vpDescending,
    textToQP "end_key" vpEndKey,
    docIdToQP "end_key_doc_id" vpEndKeyDocId,
    boolToQP "group" vpGroup,
    intToQP "group_level" vpGroupLevel,
    boolToQP "include_docs" vpIncludeDocs,
    boolToQP "inclusive_end" vpInclusiveEnd,
    textToQP "key" vpKey,
    intToQP "limit" vpLimit,
    boolToQP "reduce" vpReduce,
    intToQP "skip" vpSkip,
    boolToQP "stale" vpStale,
    textToQP "start_key" vpStartKey,
    docIdToQP "start_key_doc_id" vpStartKeyDocId,
    boolToQP "update_seq" vpUpdateSeq
    ]

-- | The default (empty) parameters
viewParams :: ViewParams
viewParams = ViewParams Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
