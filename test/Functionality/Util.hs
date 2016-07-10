{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Util where

import           Control.Monad                    (liftM, return, (>=>), (>>=))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Aeson                       (FromJSON, Value)
import           Data.Default                     (def)
import           Data.Either                      (Either (Left, Right))
import           Data.Function                    (const, id, ($), (.))
import           Data.Functor                     (fmap, (<$>))
import           Data.JsonSchema.Draft4           (FilesystemValidationFailure (..),
                                                   SchemaWithURI (..),
                                                   emptySchema,
                                                   fetchFilesystemAndValidate,
                                                   _schemaRef)
import           Data.Maybe                       (Maybe (Just, Nothing))
import           Data.Monoid                      ((<>))
import           Data.String                      (IsString, String, fromString,
                                                   unwords)
import           Data.Text                        (pack)
import           Data.UUID                        (toString)
import qualified Database.Couch.Explicit.Database as Database (create, delete)
import qualified Database.Couch.Response          as Response (asBool)
import           Database.Couch.Types             (Context (Context), Error,
                                                   Port (Port), Result)
import           GHC.Err                          (error)
import           Network.HTTP.Client              (Manager,
                                                   defaultManagerSettings,
                                                   newManager)
import           System.FilePath                  ((</>))
import           System.IO                        (FilePath, IO)
import           System.Random                    (randomIO)
import           Test.Tasty                       (TestName, TestTree,
                                                   defaultMain, testGroup,
                                                   withResource)
import           Test.Tasty.HUnit                 (assertFailure, testCaseSteps,
                                                   (@=?))
import           Text.Show                        (show)

dbContext :: MonadIO m => IO Manager -> m Context
dbContext getManager = do
  manager <- liftIO getManager
  uuid <- liftM (fromString . ("test-" <>) . toString) (liftIO randomIO)
  return $ Context manager "localhost" (Port 5984) Nothing def (Just uuid)

serverContext :: MonadIO m => IO Manager -> m Context
serverContext getManager = do
  manager <- liftIO getManager
  return $ Context manager "localhost" (Port 5984) Nothing def Nothing

releaseContext :: Context -> IO ()
releaseContext = const $ return ()

runTests :: (IO Manager -> TestTree) -> IO ()
runTests testTree = do
  let manager = newManager defaultManagerSettings
  defaultMain $ testTree manager

testAgainstFailure :: String
                   -> (Context -> IO (Result Value))
                   -> Error
                   -> IO Context
                   -> TestTree
testAgainstFailure desc function exception getContext = testCaseSteps desc $ \step -> do
  step "Make request"
  getContext >>= function >>= checkException step exception

checkException :: (String -> IO ())
               -> Error
               -> Result Value
               -> IO ()
checkException step exception res = do
  step "Got an exception"
  case res of
    -- HttpException isn't Eqable, so we simply coerce with show
    Left err -> show exception @=? show err
    Right val -> assertFailure $ unwords
                                   [ "Didn't get expected exception"
                                   , show exception
                                   , "instead"
                                   , show val
                                   ]

throwOnError :: FromJSON a => Result a -> IO ()
throwOnError res =
  case res of
    Left err -> error $ show err
    Right _  -> return ()

withDb :: (IO Context -> TestTree) -> IO Context -> TestTree
withDb test getContext =
  withResource
    (getContext >>= createTempDb)
    (fmap Response.asBool . Database.delete >=> throwOnError)
    test
  where
    createTempDb ctx = do
      Response.asBool <$> Database.create ctx >>= throwOnError
      return ctx

testAgainstSchema :: String
                  -> (Context -> IO (Result Value))
                  -> FilePath
                  -> IO Context
                  -> TestTree
testAgainstSchema desc function schema =
  testAgainstSchemaAndValue desc function schema id (const . const (return ()))

testAgainstSchemaAndValue :: String
                          -> (Context -> IO (Result Value))
                          -> FilePath
                          -> (Result Value -> Result a)
                          -> ((String -> IO ()) -> a -> IO ())
                          -> IO Context
                          -> TestTree
testAgainstSchemaAndValue desc function schema decoder checker getContext = testCaseSteps desc $ \step -> do
  step "Make request"
  getContext >>= function >>= checkCookiesAndSchema step schema decoder checker

checkCookiesAndSchema :: (String -> IO ())
                      -> FilePath
                      -> (Result Value -> Result a)
                      -> ((String -> IO ()) -> a -> IO ())
                      -> Result Value
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
  let schema = SchemaWithURI { _swSchema = emptySchema {_schemaRef = Just $ pack schemaName}, _swURI = Just $ pack $ "test/schema/schema" </> schemaName}
  res <- liftIO $ fetchFilesystemAndValidate schema value
  case res of
    Right () -> return ()
    Left (FVRead _) -> error "Error fetching a referenced schema (either during IO or parsing)."
    Left (FVSchema _) -> error "Our 'schema' itself was invalid."
    Left f@(FVData _) -> assertFailure $ unwords ["Failed to validate", show value, ":", show f]

class TestInput a where
  makeTests :: TestName -> [IO Context -> TestTree] -> a -> TestTree
  makeTests desc tests input = testGroup desc $ fmap (applyInput input) tests

  applyInput :: a -> (IO Context -> TestTree) -> TestTree

instance TestInput (IO Manager) where
  applyInput input = ($ dbContext input)

instance TestInput (IO Context) where
  applyInput input = ($ input)
