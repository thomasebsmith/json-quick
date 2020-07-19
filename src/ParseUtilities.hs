module ParseUtilities
( takeString
, takeString'
, takeWhitespace
, takeWhile
, takeUntil
, takeOnly
, takeChar
, peekChar
, isIdentifierChar
, isWhitespace
, ParseError
) where

import Prelude hiding (takeWhile)
import Control.Monad.Trans.State.Lazy
import Data.Char
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

type ParseError = String

-- Take a string from input. Requires that an opening " has been detected.
takeString :: State B.ByteString (Either ParseError C.ByteString)
takeString = do
  _ <- takeChar
  takeString'

takeString' :: State B.ByteString (Either ParseError C.ByteString)
takeString' = do
  maybeChar <- takeChar
  case maybeChar of
    Nothing -> return $ Left "Unexpected EOF while reading string"
    Just '\\' -> do
      escaped <- takeEscape
      remaining <- takeString'
      return $ C.cons '\\' <$> (C.append <$> escaped <*> remaining)
    Just '"' -> do
      return $ Right C.empty
    Just char -> do
      remaining <- takeString'
      return $ (char `C.cons`) <$> remaining

takeEscape :: State B.ByteString (Either ParseError C.ByteString)
takeEscape = do
  escapeType <- takeChar
  case escapeType of
    Nothing -> return $ Left "Unexpected EOF while reading string escape"
    Just 'u' -> do
      hexDigits <- takeHexEscape
      return $ C.cons 'u' <$> hexDigits
    Just escape -> if isSingleEscapeChar escape
                      then return $ Right $ escape `C.cons` C.singleton escape
                      else return $ Left $
                        "Invalid string escape with character " ++ show escape

takeHexEscape :: State B.ByteString (Either ParseError C.ByteString)
takeHexEscape = do
  maybeDigits <- (sequence . take 4 . repeat) takeChar
  case sequence maybeDigits of
    Just digits -> if all isHexDigit digits
                      then return $ Right $ C.pack digits
                      else return $ Left $
                        "Invalid unicode escape with digits " ++ digits
    Nothing -> return $ Left "Unexpected EOF while reading unicode escape"

isSingleEscapeChar :: Char -> Bool
isSingleEscapeChar '"' = True
isSingleEscapeChar '\\' = True
isSingleEscapeChar '/' = True
isSingleEscapeChar 'b' = True
isSingleEscapeChar 'f' = True
isSingleEscapeChar 'n' = True
isSingleEscapeChar 'r' = True
isSingleEscapeChar 't' = True
isSingleEscapeChar _ = False



takeWhitespace :: State B.ByteString ()
takeWhitespace = do
  char <- peekChar
  case isWhitespace <$> char of
    Just True -> do
      _ <- takeChar
      takeWhitespace
    _ -> do
      return ()

-- Consumes and returns all characters while condition succeeds or an EOF is
-- reached.
takeWhile :: (Char -> Bool) -> State B.ByteString C.ByteString
takeWhile condition = takeUntil (not . condition)

-- Consumes and returns all characters before condition fails or an EOF is
-- reached.
takeUntil :: (Char -> Bool) -> State B.ByteString C.ByteString
takeUntil condition = do
  maybeChar <- peekChar
  case maybeChar of
    Nothing -> return C.empty
    Just nextChar -> if condition nextChar
                        then return C.empty
                        else do
                          _ <- takeChar
                          rest <- takeUntil condition
                          return $ nextChar `C.cons` rest

takeOnly :: String -> State B.ByteString Bool
takeOnly "" = return True
takeOnly (char:chars) = do
  next <- takeChar
  case next of
    Just nextChar -> if char == nextChar
                        then takeOnly chars
                        else return False
    _ -> return False

peekChar :: State B.ByteString (Maybe Char)
peekChar = state $ \input ->
  case C.uncons input of
    Nothing -> (Nothing, input)
    Just (char, _) -> (Just char, input)

takeChar :: State B.ByteString (Maybe Char)
takeChar = state $ \input ->
  case C.uncons input of
    Nothing -> (Nothing, input)
    Just (char, chars) -> (Just char, chars)

isIdentifierChar :: Char -> Bool
isIdentifierChar char
  | isAlphaNum char = True
  | char == '$' = True
  | char == '_' = True
  | otherwise = False

isWhitespace :: Char -> Bool
isWhitespace char = case char of
                      ' ' -> True
                      '\n' -> True
                      '\t' -> True
                      '\r' -> True
                      _ -> False
