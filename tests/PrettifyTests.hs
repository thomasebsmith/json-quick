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
    it "prettifies values with extra whitespace" $ do
      testPrettify "    [  \t  3.14   \r\n\n\n  ,\t \" \"]" `shouldBe`
                   "[\n  3.14,\n  \" \"\n]\n"
      testPrettify "\t{ \" a key \"\n:\n   true,\n    \" a key \":5}\n\n"
                   `shouldBe`
                   "{\n  \" a key \": true,\n  \" a key \": 5\n}\n"
    it "prettifies empty arrays and objects" $ do
      testPrettify "[]" `shouldBe` "[]\n"
      testPrettify "[[], []]" `shouldBe` "[\n  [],\n  []\n]\n"
      testPrettify "{ \"key\":[], \"key2\":[{}]  } " `shouldBe`
                   "{\n  \"key\": [],\n  \"key2\": [\n    {}\n  ]\n}\n"
    it "prettifies nested arrays and objects" $ do
      testPrettify "[[[[[3,1,4],1]]],5]" `shouldBe`
                   "[\n\
                   \  [\n\
                   \    [\n\
                   \      [\n\
                   \        [\n\
                   \          3,\n\
                   \          1,\n\
                   \          4\n\
                   \        ],\n\
                   \        1\n\
                   \      ]\n\
                   \    ]\n\
                   \  ],\n\
                   \  5\n\
                   \]\n"
    it  "prettifies strings with escapes" $ do
      testPrettify "[  \"3.14159  \\\"  ,3]\"]" `shouldBe`
                   "[\n  \"3.14159  \\\"  ,3]\"\n]\n"

testPrettify :: String -> String
testPrettify = C.unpack . prettify . C.pack
