module Prettify
( prettify
) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

data JSONToken = String | StringEscape | Value deriving (Enum)
data JSONState = JSONState
  { token :: JSONToken
  , currentIndent :: C.ByteString
  }

prettify :: B.ByteString -> B.ByteString
prettify input = prettify' input initialState
  where initialState = JSONState { token = Value, currentIndent = C.empty }

prettify' :: B.ByteString -> JSONState -> B.ByteString
prettify' input state = output
  where output = case C.uncons input of
                   Just (char, chars) -> replacement `C.append` remaining
                     where remaining = prettify' chars newState
                           (replacement, newState) = process char state
                   Nothing -> C.singleton '\n'

process :: Char -> JSONState -> (C.ByteString, JSONState)
process char state =
  case token state of
    StringEscape -> (C.singleton char, state { token = String })
    String -> case char of
                '\\' -> (C.singleton char, state { token = StringEscape })
                '"'  -> (C.singleton char, state { token = Value })
                otherwise -> (C.singleton char, state)
    Value -> case char of
               '"' -> (C.singleton char, state { token = String })
               '[' -> increaseIndent
               '{' -> increaseIndent
               ']' -> decreaseIndent
               '}' -> decreaseIndent
               ',' -> lineBreak
               ':' -> addSpace
               ' ' -> ignore
               '\n' -> ignore
               '\r' -> ignore
               '\t' -> ignore
               otherwise -> (C.singleton char, state)
  where increasedIndent = singleIndent `C.append` currentIndent state
        decreasedIndent = C.drop (C.length singleIndent) $ currentIndent state
        increaseIndent =
          ( char `C.cons` '\n' `C.cons` increasedIndent
          , state { currentIndent = increasedIndent }
          )
        decreaseIndent =
          ( ('\n' `C.cons` decreasedIndent) `C.append` C.singleton char
          , state { currentIndent = decreasedIndent }
          )
        lineBreak = (char `C.cons` '\n' `C.cons` currentIndent state, state)
        addSpace = (char `C.cons` ' ' `C.cons` C.empty, state)
        ignore = (C.empty, state)

singleIndent :: C.ByteString
singleIndent = C.pack "  "
