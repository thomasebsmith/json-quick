module Verify
( verify
) where

import Parse
import qualified Data.ByteString.Lazy as B

verify :: B.ByteString -> Maybe ParseError
verify = succeeded . parse
