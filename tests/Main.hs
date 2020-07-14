module Main where

import Test.Hspec
import qualified PrettifyTests

main :: IO ()
main = hspec $ do
  PrettifyTests.spec
