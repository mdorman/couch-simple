{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Util where

import           Control.Monad                    (liftM, return, (>=>), (>>=))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Aeson                       (Value (Object), decode)
import           Data.ByteString.Lazy             (readFile)
import           Data.Default                     (def)
import           Data.Either                      (Either (Left, Right))
import           Data.Eq                          ((==))
import           Data.Function                    (const, id, ($), (.))
import           Data.JsonSchema                  (RawSchema (..), compile,
                                                   draft4, validate)
import           Data.Maybe                       (Maybe (Just, Nothing))
import           Data.Monoid                      (mempty, (<>))
import           Data.String                      (IsString, String, fromString)
import           Data.UUID                        (toString)
import qualified Database.Couch.Explicit.Database as Database (create, delete)
import           Database.Couch.Types             (Context (Context),
                                                   CouchError (..), Port (Port),
                                                   ctxManager)
import           GHC.Err                          (error)
import           Network.HTTP.Client              (CookieJar, Manager,
                                                   closeManager,
                                                   defaultManagerSettings,
                                                   withManager)
import           System.Directory                 (doesFileExist,
                                                   getCurrentDirectory)
import           System.FilePath                  (takeDirectory, (</>))
import           System.IO                        (FilePath, IO)
import           System.Random                    (randomIO)
import           Test.Tasty                       (TestTree, defaultMain,
                                                   withResource)
import           Test.Tasty.HUnit                 (assertFailure, testCaseSteps,
                                                   (@=?))
import           Text.Show                        (show)

dbContext :: MonadIO m => Manager -> m Context
dbContext manager = do
  uuid <- liftM (fromString . ("test-" <>) . toString) (liftIO randomIO)
  return $ Context manager "localhost" (Port 5984) Nothing def (Just uuid)

serverContext :: MonadIO m => Manager -> m Context
serverContext manager = return $ Context manager "localhost" (Port 5984) Nothing def Nothing

releaseContext :: Context -> IO ()
releaseContext = closeManager . ctxManager

runTests :: (Manager -> TestTree) -> IO ()
runTests tests = withManager defaultManagerSettings (defaultMain . tests)

testAgainstFailure :: String
                  -> (Context -> IO (Either CouchError (Value, Maybe CookieJar)))
                  -> CouchError
                  -> IO Context
                  -> TestTree
testAgainstFailure desc function exception getContext = testCaseSteps desc $ \step -> do
  step "Make request"
  getContext >>= function >>= checkException step exception

checkException :: (String -> IO ())
               -> CouchError
               -> Either CouchError (Value, Maybe CookieJar)
               -> IO ()
checkException step exception res = do
  step "Got an exception"
  case res of
    -- HttpException isn't Eqable, so we simply coerce with show
    Left err -> show exception @=? show err
    Right _ -> assertFailure "Didn't get expected exception"

throwOnError :: Either CouchError (a, Maybe CookieJar) -> IO ()
throwOnError res =
  case res of
   Left err -> error $ show err
   Right _ -> return ()

withDb :: IO Context -> (IO Context -> TestTree) -> TestTree
withDb getContext =
  withResource (getContext >>= createTempDb) (Database.delete >=> throwOnError)
  where
    createTempDb ctx = do
      Database.create ctx >>= throwOnError
      return ctx

testAgainstSchema :: String
                  -> (Context -> IO (Either CouchError (Value, Maybe CookieJar)))
                  -> FilePath
                  -> IO Context
                  -> TestTree
testAgainstSchema desc function schema =
  testAgainstSchemaAndValue desc function schema id (const . const (return ()))

testAgainstSchemaAndValue :: String
                          -> (Context -> IO (Either CouchError (Value, Maybe CookieJar)))
                          -> FilePath
                          -> (Either CouchError (Value, Maybe CookieJar) -> Either CouchError (a, Maybe CookieJar))
                          -> ((String -> IO ()) -> a -> IO ())
                          -> IO Context
                          -> TestTree
testAgainstSchemaAndValue desc function schema decoder checker getContext = testCaseSteps desc $ \step -> do
  step "Make request"
  getContext >>= function >>= checkCookiesAndSchema step schema decoder checker

checkCookiesAndSchema :: (String -> IO ())
                      -> FilePath
                      -> (Either CouchError (Value, Maybe CookieJar) -> Either CouchError (a, Maybe CookieJar))
                      -> ((String -> IO ()) -> a -> IO ())
                      -> Either CouchError (Value, Maybe CookieJar)
                      -> IO ()
checkCookiesAndSchema step schemaFile decoder checker res = do
  step "No exception"
  case res of
    Left err -> assertFailure (show err)
    Right (json, cookieJar) -> do
      step "Empty cookie jar"
      def @=? cookieJar
      checkSchema step json schemaFile
      step $ "Decoding json: " <> show json
      case decoder res of
        Left err -> assertFailure (show err)
        Right (val, _) -> do
          step "Checking value"
          checker step val

checkSchema :: IsString s => (s -> IO ()) -> Value -> FilePath -> IO ()
checkSchema step value schemaName = do
  step "Checking result against schema"
  schema <- loadSchema ("test/schema/schema" </> schemaName)
  case validate (compile draft4 mempty schema) value of
    Left err -> assertFailure (show err)
    Right _ -> return ()

loadSchema :: FilePath -> IO RawSchema
loadSchema file = do
  (_, content) <- findRequestedFile
  case decode content of
    Just (Object o) -> return RawSchema { _rsURI = "", _rsObject = o }
    _               -> error "Couldn't extract object from file"

  where
    findRequestedFile = do
      start <- getCurrentDirectory
      checkDir start
    checkDir dir =
      let path = dir </> file
      in doesFileExist path >>= \exists -> if exists
                                             then readFile path >>= \c -> return (path, c)
                                             else let upOne = takeDirectory dir
                                                  in if upOne == dir
                                                       then error "Cannot find file to embed as resource"
                                                       else checkDir upOne
