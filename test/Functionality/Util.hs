{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Functionality.Util where

import           Control.Monad          (liftM, return, (>>=))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (Value (Object), decode)
import           Data.ByteString.Lazy   (readFile)
import           Data.Default           (def)
import           Data.Either            (Either (Left, Right))
import           Data.Eq                ((==))
import           Data.Function          (($), (.))
import           Data.JsonSchema        (RawSchema (..), compile, draft4,
                                         validate)
import           Data.Maybe             (Maybe (Just, Nothing))
import           Data.Monoid            (mempty, (<>))
import           Data.String            (IsString, String, fromString)
import           Data.UUID              (toString)
import           Database.Couch.Types   (Context (Context), CouchError (..),
                                         Port (Port), ctxManager)
import           GHC.Err                (error)
import           Network.HTTP.Client    (CookieJar, Manager, closeManager,
                                         defaultManagerSettings, withManager)
import           System.Directory       (doesFileExist, getCurrentDirectory)
import           System.FilePath        (takeDirectory, (</>))
import           System.IO              (FilePath, IO)
import           System.Random          (randomIO)
import           Test.Tasty             (TestTree, defaultMain)
import           Test.Tasty.HUnit       (assertFailure, testCaseSteps, (@=?))
import           Text.Show              (show)

dbContext :: MonadIO m => Manager -> m Context
dbContext manager = do
  uuid <- liftM (fromString . ("aaa" <>) . toString) (liftIO randomIO)
  return $ Context manager "localhost" (Port 5984) Nothing def (Just uuid)

serverContext :: MonadIO m => Manager -> m Context
serverContext manager = return $ Context manager "localhost" (Port 5984) Nothing def Nothing

releaseContext :: Context -> IO ()
releaseContext =
  closeManager . ctxManager

runTests :: (Manager -> TestTree) -> IO ()
runTests tests = withManager defaultManagerSettings (defaultMain . tests)

testAgainstSchema :: String -> (Context -> IO (Either CouchError (Value, Maybe CookieJar))) -> FilePath -> IO Context -> TestTree
testAgainstSchema desc function schema getContext = testCaseSteps desc $ \step -> do
  step "Make request"
  getContext >>= function >>= checkCookiesAndSchema step schema

checkCookiesAndSchema :: (String -> IO ()) -> FilePath -> Either CouchError (Value, Maybe CookieJar) -> IO ()
checkCookiesAndSchema step schemaFile res = do
  step "No exception"
  case res of
    Left err -> assertFailure (show err)
    Right (value, cookieJar) -> do
      step "Empty cookie jar"
      def @=? cookieJar
      checkSchema step value schemaFile

checkSchema :: IsString s => (s -> IO ()) -> Value -> FilePath -> IO ()
checkSchema step value schemaName = do
  step "Checking result against schema"
  schema <- loadSchema ("test/schema" </> schemaName)
  validate (compile draft4 mempty schema) value @=? mempty

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
