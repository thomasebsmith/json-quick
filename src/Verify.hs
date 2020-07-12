module Verify
( verify
) where

import Parse
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

verify :: B.ByteString -> Maybe ParseError
verify = succeeded . parse
