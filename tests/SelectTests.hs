module SelectTests
( spec
) where

import Parse
import Select
import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as C

spec :: Spec
spec = do
  describe "select" $ do
    it "selects entire values" $ do
      testSelect " " "\"Some\\nstring\"" `shouldBe` Just "\"Some\\nstring\"\n"
      testSelect "" "100E3" `shouldBe` Just "100E3\n"
      testSelect "\n" "true" `shouldBe` Just "true\n"
      testSelect "\r" " false\t" `shouldBe` Just "false\n"
      testSelect "   " "\rnull" `shouldBe` Just "null\n"
      testSelect "" "[1, 2, 3]" `shouldBe` Just "[1,2,3]\n"
      testSelect "" "{\"key\": 89}" `shouldBe` Just "{\"key\":89}\n"
    it "fails to select invalid JSON regions" $ do
      testSelect "" "blah" `shouldBe` Nothing
      testSelect "" "[1" `shouldBe` Nothing
      testSelect "key" "{\"key\":[" `shouldBe` Nothing
      testSelect "" "{[]}" `shouldBe` Nothing
    it "selects properties" $ do
      testSelect "prop1" "{\"prop2\":3,\"prop1\":4}" `shouldBe` Just "4\n"
      testSelect "\"some key\"" "{\"some key2\":1,\"some key\":-1}" `shouldBe`
        Just "-1\n"
    it "fails to select properties that don't exist" $ do
      testSelect "notexist" "{\"exists\":3.14159}" `shouldBe` Nothing
      testSelect "property" "\"property\"" `shouldBe` Nothing
      testSelect "key" "[1,2,3]" `shouldBe` Nothing
    it "selects indices" $ do
      testSelect "0" "[-1,-2]" `shouldBe` Just "-1\n"
      testSelect "2" "[{},[],[]]" `shouldBe` Just "[]\n"
      testSelect "1" "[0,{\"key\":3.14}]" `shouldBe` Just "{\"key\":3.14}\n"
    it "fails to select indices that don't exist" $ do
      testSelect "0" "[]" `shouldBe` Nothing
      testSelect "2" "[1,2]" `shouldBe` Nothing
      testSelect "0a" "[3,1,4]" `shouldBe` Nothing
    it "selects all properties" $ do
      testSelect "*" "[3,1,4,[1,5]]" `shouldBe` Just "[3,1,4,[1,5]]\n"
      testSelect "*" "{\"k\":3,\"k2\":4}" `shouldBe` Just "[3,4]\n"
    it "selects nested properties and indices" $ do
      testSelect "prop.1.*" "{\"p\":3,\"prop\":[0,{\"a\":1,\"b\":2}]}"
        `shouldBe` Just "[1,2]\n"
      testSelect "\"key\".\"*\".*" "{\"key\":{\"*\":[1,2,3]}}\n" `shouldBe`
        Just "[1,2,3]\n"

testSelect :: String -> String -> Maybe String
testSelect pattern input = case selected of
                             Left _ -> Nothing
                             Right value -> C.unpack <$> showAsByteString value
  where selected = parseAndSelect (C.pack input) (C.pack pattern) 
