module Prettify
( prettify
) where

import qualified Data.ByteString.Lazy as B

prettify :: B.ByteString -> B.ByteString
prettify = id
