{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Util where

import           Control.Monad                    (liftM, return, (>=>), (>>=))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Aeson                       (FromJSON, Value (Object),
                                                   decode)
import           Data.ByteString.Lazy             (readFile)
import           Data.Default                     (def)
import           Data.Either                      (Either (Left, Right))
import           Data.Eq                          ((==))
import           Data.Function                    (const, id, ($), (.))
import           Data.Functor                     (fmap, (<$>))
import           Data.JsonSchema                  (RawSchema (..), compile,
                                                   draft4, validate)
import           Data.Maybe                       (Maybe (Just, Nothing))
import           Data.Monoid                      (mempty, (<>))
import           Data.String                      (IsString, String, fromString,
                                                   unwords)
import           Data.UUID                        (toString)
import qualified Database.Couch.Explicit.Database as Database (create, delete)
import qualified Database.Couch.Response          as Response (asBool)
import           Database.Couch.Types             (Context (Context), Error,
                                                   Port (Port), Result)
import           GHC.Err                          (error)
import           Network.HTTP.Client              (Manager,
                                                   defaultManagerSettings,
                                                   newManager)
import           System.Directory                 (doesFileExist,
                                                   getCurrentDirectory)
import           System.FilePath                  (takeDirectory, (</>))
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
  schema <- loadSchema ("test/schema/schema" </> schemaName)
  case validate (compile draft4 mempty schema) value of
    Left err -> assertFailure $ unwords ["Failed to validate", show value, ":", show err]
    Right _  -> return ()

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

class TestInput a where
  makeTests :: TestInput a => TestName -> [IO Context -> TestTree] -> a -> TestTree
  makeTests desc tests input = testGroup desc $ fmap (applyInput input) tests

  applyInput :: a -> (IO Context -> TestTree) -> TestTree

instance TestInput (IO Manager) where
  applyInput input = ($ dbContext input)

instance TestInput (IO Context) where
  applyInput input = ($ input)
