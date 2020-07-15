module VerifyTests
( spec
) where

import Data.Maybe
import Test.Hspec
import Parse (ParseError)
import Verify
import qualified Data.ByteString.Lazy.Char8 as C

spec :: Spec
spec = do
  describe "prettify" $ do
    it "verifies basic values" $ do
      testVerify "null" `shouldSatisfy` isNothing
      testVerify "true" `shouldSatisfy` isNothing
      testVerify "false" `shouldSatisfy` isNothing
      testVerify "\" a string. \"" `shouldSatisfy` isNothing
      testVerify "[3, \n1]" `shouldSatisfy` isNothing
      testVerify "{\"key\":314}" `shouldSatisfy` isNothing
      testVerify "-0.89" `shouldSatisfy` isNothing
      testVerify "[{},[]]" `shouldSatisfy` isNothing
    it "fails to verify basic errors" $ do
      testVerify "Null" `shouldSatisfy` isJust
      testVerify "truee" `shouldSatisfy` isJust
      testVerify "[3,]" `shouldSatisfy` isJust
      testVerify "," `shouldSatisfy` isJust
      testVerify "#" `shouldSatisfy` isJust
      testVerify "\"a \\DNE escape\"" `shouldSatisfy` isJust
      testVerify "\"unclosed" `shouldSatisfy` isJust
      testVerify "[]]" `shouldSatisfy` isJust
      testVerify "[[]" `shouldSatisfy` isJust
      testVerify "00.3" `shouldSatisfy` isJust
    it "verifies escaped strings" $ do
      testVerify "[ \"this string\\\" is escaped \\uA09F \\n \", 3]"
                 `shouldSatisfy` isNothing

testVerify :: String -> Maybe ParseError
testVerify = verify . C.pack
