module Verify
( verify
) where

import Parse
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

verify :: B.ByteString -> Bool
verify = succeeded . parse
