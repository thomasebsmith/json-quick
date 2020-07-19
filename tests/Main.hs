module Main where

import Test.Hspec
import qualified PrettifyTests
import qualified SelectTests
import qualified VerifyTests

main :: IO ()
main = hspec $ do
  PrettifyTests.spec
  SelectTests.spec
  VerifyTests.spec
