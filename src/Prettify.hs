module Prettify
( prettify
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as CS
import qualified Data.ByteString.Lazy as B

data JSONToken = String | StringEscape | Value deriving (Enum)
data JSONState = JSONState
  { token :: JSONToken
  , currentIndent :: String
  }

prettify :: B.ByteString -> B.ByteString
prettify input = output
  where output = B.fromChunks chunks
        (chunks, _) = B.foldrChunks accumulator initial input
        initial =
          ( [CS.singleton '\n']
          , JSONState { token = Value, currentIndent = "" }
          )
        accumulator = \chunk (chunks, state) ->
          let (newChunk, newState) = prettify' chunk state in
              (newChunk:chunks, newState)

prettify' :: S.ByteString -> JSONState -> (S.ByteString, JSONState)
prettify' string state = (CS.pack chars, finalState)
  where (chars, finalState) = analyze string state

singleIndent :: String
singleIndent = "  "

analyze :: S.ByteString -> JSONState -> (String, JSONState)
analyze string state =
  case CS.uncons string of
    Just (char, chars) -> (prefix ++ suffix, finalState)
      where (suffix, finalState) = analyze chars nextState
            (prefix, nextState) = prettifyStep char state
    Nothing -> ("", state)

prettifyStep :: Char -> JSONState -> (String, JSONState)
prettifyStep char state =
  case token state of
    StringEscape -> ([char], state { token = String })
    String -> case char of
                '\\' -> ([char], state { token = StringEscape })
                '"'  -> ([char], state { token = Value })
                otherwise -> ([char], state)
    Value -> case char of
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
               otherwise -> ([char], state)
  where increasedIndent = singleIndent ++ currentIndent state
        decreasedIndent = drop (length singleIndent) $ currentIndent state
        increaseIndent =
          ( char:'\n':increasedIndent
          , state { currentIndent = increasedIndent }
          )
        decreaseIndent =
          ( '\n':decreasedIndent ++ [char]
          , state { currentIndent = decreasedIndent }
          )
        lineBreak = (char:'\n':currentIndent state, state)
        addSpace = (char:' ':"", state)
        ignore = ("", state)
