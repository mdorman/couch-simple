{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Data.List              (null)
import           Data.String            (String)
import           Language.Haskell.HLint (hlint)
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO              (IO)

arguments :: [String]
arguments =
  ["src", "test"]

main :: IO ()
main = do
  hints <- hlint arguments
  if null hints
    then exitSuccess
    else exitFailure
