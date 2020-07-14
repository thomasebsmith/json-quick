module PrettifyTests
( spec
) where

import Prettify
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as C

spec :: Spec
spec = do
  describe "prettify" $ do
    it "prettifies basic values" $ do
      testPrettify "\"A string.\"" `shouldBe` "\"A string.\"\n"
      testPrettify "-31.4159e-1" `shouldBe` "-31.4159e-1\n"
      testPrettify "true" `shouldBe` "true\n"
      testPrettify "false" `shouldBe` "false\n"
      testPrettify "null" `shouldBe` "null\n"
      testPrettify "[3,1,4]" `shouldBe` "[\n  3,\n  1,\n  4\n]\n"
      testPrettify "{\"key\":\"value\"}" `shouldBe`
                   "{\n  \"key\": \"value\"\n}\n"

testPrettify :: String -> String
testPrettify = C.unpack . prettify . C.pack
