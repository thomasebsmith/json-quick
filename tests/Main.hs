module Main where

import Test.Hspec
import qualified PrettifyTests
import qualified VerifyTests

main :: IO ()
main = hspec $ do
  PrettifyTests.spec
  VerifyTests.spec
