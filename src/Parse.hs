module Parse
( parse
, succeeded
, JSONValue (..)
) where

import Prelude hiding (takeWhile)

import Control.Monad.Trans.State.Lazy
import Data.Char
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified FailList as F

type ParseResult = (Maybe JSONValue, B.ByteString)
type JSONPair = (C.ByteString, JSONValue)
data JSONValue = JSONObject (F.FailList JSONPair)
               | JSONArray (F.FailList JSONValue)
               | JSONNumber C.ByteString
               | JSONString C.ByteString
               | JSONTrue
               | JSONFalse | JSONNull

parse :: B.ByteString -> ParseResult
parse = runState takeValue

succeeded :: ParseResult -> Bool
succeeded result = case result of
                     (Just _, rest) ->
                       let nonWhitespace = snd $ runState takeWhitespace rest in
                           B.null nonWhitespace
                     _ -> False

takeValue :: State B.ByteString (Maybe JSONValue)
takeValue = do
  takeWhitespace
  next <- peekChar
  case next of
    Nothing -> return Nothing
    Just char
      | char == '{' -> takeObject
      | char == '[' -> takeArray
      | char == '-' -> takeNegativeNumber
      | isDigit char -> takeNumber
      | char == '"' -> takeString
      | char == 't' -> takeTrue
      | char == 'f' -> takeFalse
      | char == 'n' -> takeNull
      | otherwise -> return Nothing

takeTrue :: State B.ByteString (Maybe JSONValue)
takeTrue = do
  succeeded <- takeOnly "true"
  if succeeded
     then return $ Just JSONTrue
     else return Nothing

takeFalse :: State B.ByteString (Maybe JSONValue)
takeFalse = do
  succeeded <- takeOnly "false"
  if succeeded
     then return $ Just JSONFalse
     else return Nothing

takeNull :: State B.ByteString (Maybe JSONValue)
takeNull = do
  succeeded <- takeOnly "null"
  if succeeded
     then return $ Just JSONNull
     else return Nothing

takeNegativeNumber :: State B.ByteString (Maybe JSONValue)
takeNegativeNumber = do
  takeChar -- the minus sign
  num <- takeNumber
  case num of
    Just (JSONNumber str) -> return $ Just $ JSONNumber $ '-' `C.cons` str
    _ -> return Nothing

takeNumber :: State B.ByteString (Maybe JSONValue)
takeNumber = do
  maybeDigit <- peekChar
  case maybeDigit of
    Nothing -> return Nothing
    Just '0' -> do
      takeChar
      decimal <- takeDecimal
      return $ JSONNumber <$> ('0' `C.cons`) <$> decimal
    Just char -> do
      num <- takeNumber'
      return $ JSONNumber <$> num

takeNumber' :: State B.ByteString (Maybe C.ByteString)
takeNumber' = do
  maybeDigit <- peekChar
  case maybeDigit of
    Nothing -> return $ Just C.empty
    Just char
      | isDigit char -> do
          takeChar
          remaining <- takeNumber'
          return $ (char `C.cons`) <$> remaining
      | otherwise -> takeDecimal

takeDecimal :: State B.ByteString (Maybe C.ByteString)
takeDecimal = do
  maybeDot <- peekChar
  case maybeDot of
    Nothing -> return $ Just C.empty
    Just '.' -> do
      takeChar
      decimal <- takeDecimal'
      return $ ('.' `C.cons`) <$> decimal
    _ -> takeExponent

takeDecimal' :: State B.ByteString (Maybe C.ByteString)
takeDecimal' = do
  maybeDigit <- takeChar
  case maybeDigit of
    Nothing -> return Nothing
    Just digit
      | isDigit digit -> do
          next <- peekChar
          case next of
            Just nextChar
              | isDigit nextChar -> do
                  decimal <- takeDecimal'
                  return $ (digit `C.cons`) <$> decimal
              | otherwise -> do
                  exponent <- takeExponent
                  return $ (digit `C.cons`) <$> exponent
            Nothing -> return $ Just $ C.singleton digit
      | otherwise -> return Nothing

takeExponent :: State B.ByteString (Maybe C.ByteString)
takeExponent = do
  maybeE <- peekChar
  case maybeE of
    Nothing -> return $ Just $ C.empty
    Just char
      | char == 'e' || char == 'E' -> do
          takeChar
          exponent <- takeExponent'
          return $ (char `C.cons`) <$> exponent
      | otherwise -> return $ Just $ C.empty

takeExponent' :: State B.ByteString (Maybe C.ByteString)
takeExponent' = do
  maybeSign <- peekChar
  case maybeSign of
    Nothing -> return Nothing
    Just sign
      | sign == '+' || sign == '-' -> do
          takeChar
          exponent <- takeExponent''
          return $ (sign `C.cons`) <$> exponent
      | otherwise -> takeExponent''

takeExponent'' :: State B.ByteString (Maybe C.ByteString)
takeExponent'' = do
  maybeDigit <- takeChar
  case maybeDigit of
    Nothing -> return Nothing
    Just digit
      | isDigit digit -> do
          digits <- takeWhile isDigit
          return $ (digit `C.cons`) <$> digits
      | otherwise -> return Nothing

-- Take an array from input. Requires that an opening [ has been detected.
takeArray :: State B.ByteString (Maybe JSONValue)
takeArray = Just <$> (JSONArray <$> takeArray')

takeArray' :: State B.ByteString (F.FailList JSONValue)
takeArray' = do
  takeWhitespace
  maybeChar <- takeChar
  case maybeChar of
    Nothing -> return F.Fail
    Just char
      | char == ']' -> return F.Empty
      | char == '[' || char == ',' -> do
          maybeValue <- takeValue
          case maybeValue of
            Nothing -> return F.Fail
            Just value -> do
              remaining <- takeArray'
              return $ F.Cons value remaining
      | otherwise -> return F.Fail

-- Take a string from input. Requires that an opening " has been detected.
takeString :: State B.ByteString (Maybe JSONValue)
takeString = do
  takeChar
  maybeString <- takeString'
  return $ JSONString <$> maybeString

takeString' :: State B.ByteString (Maybe C.ByteString)
takeString' = do
  maybeChar <- takeChar
  case maybeChar of
    Nothing -> return Nothing
    Just '\\' -> do
      escaped <- takeEscape
      remaining <- takeString'
      return $ C.cons '\\' <$> (C.append <$> escaped <*> remaining)
    Just '"' -> do
      return $ Just C.empty
    Just char -> do
      remaining <- takeString'
      return $ (char `C.cons`) <$> remaining

takeEscape :: State B.ByteString (Maybe C.ByteString)
takeEscape = do
  escapeType <- takeChar
  case escapeType of
    Nothing -> return Nothing
    Just 'u' -> do
      hexDigits <- takeHexEscape
      return $ C.cons 'u' <$> hexDigits
    Just escape -> if isSingleEscapeChar escape
                      then do
                        escaped <- takeChar
                        return $ C.cons escape <$> C.singleton <$> escaped
                      else return Nothing

takeHexEscape :: State B.ByteString (Maybe C.ByteString)
takeHexEscape = do
  maybeDigits <- (sequence . take 4 . repeat) takeChar
  case sequence maybeDigits of
    Just digits -> if all isHexDigit digits
                      then return $ Just $ C.pack digits
                      else return Nothing
    Nothing -> return Nothing

isSingleEscapeChar :: Char -> Bool
isSingleEscapeChar '"' = True
isSingleEscapeChar '\\' = True
isSingleEscapeChar '/' = True
isSingleEscapeChar 'b' = True
isSingleEscapeChar 'f' = True
isSingleEscapeChar 'n' = True
isSingleEscapeChar 'r' = True
isSingleEscapeChar 't' = True
isSingleEscapeChar '_' = False

-- Takes an object from input. Requires that an initial { has been detected.
takeObject :: State B.ByteString (Maybe JSONValue)
takeObject = Just <$> JSONObject <$> takeObject'

takeObject' :: State B.ByteString (F.FailList JSONPair)
takeObject' = do
  takeWhitespace
  maybeChar <- takeChar
  case maybeChar of
    Nothing -> return F.Fail
    Just char
      | char == '}' -> return F.Empty
      | char == '{' || char == ',' -> do
          takeWhitespace
          nextChar <- takeChar
          case nextChar of
            Just '"' -> do
              maybeProp <- takeProperty
              case maybeProp of
                Just prop -> do
                  remaining <- takeObject'
                  return $ F.Cons prop remaining
                Nothing -> return F.Fail
            _ -> return F.Fail
      | otherwise -> return F.Fail

takeProperty :: State B.ByteString (Maybe (C.ByteString, JSONValue))
takeProperty = do
  maybeKey <- takeString'
  takeWhitespace
  maybeColon <- takeChar
  takeWhitespace
  case (maybeKey, maybeColon) of
    (Just key, Just ':') -> do
      maybeValue <- takeValue
      case maybeValue of
        Nothing -> return Nothing
        Just value -> do
          return $ Just (key, value)
    _ -> return Nothing

takeWhitespace :: State B.ByteString ()
takeWhitespace = do
  char <- peekChar
  case isWhitespace <$> char of
    Just True -> do
      takeChar
      takeWhitespace
    _ -> do
      return ()

-- Consumes all characters up to and including char. However, only returns
-- characters before char.
takeWithin :: Char -> State B.ByteString (Maybe C.ByteString)
takeWithin char = do
  maybeChar <- takeChar
  case maybeChar of
    Nothing -> return Nothing
    Just nextChar -> if char == nextChar
                        then return $ Just $ C.empty
                        else do
                          maybeRest <- takeWithin char
                          return $ (nextChar `C.cons`) <$> maybeRest

-- Consumes and returns all characters while condition succeeds.
takeWhile :: (Char -> Bool) -> State B.ByteString (Maybe C.ByteString)
takeWhile condition = takeUntil (not . condition)

-- Consumes and returns all characters before condition fails.
takeUntil :: (Char -> Bool) -> State B.ByteString (Maybe C.ByteString)
takeUntil condition = do
  maybeChar <- peekChar
  case maybeChar of
    Nothing -> return Nothing
    Just nextChar -> if condition nextChar
                        then return $ Just $ C.empty
                        else do
                          takeChar
                          maybeRest <- takeUntil condition
                          return $ (nextChar `C.cons`) <$> maybeRest

takeOnly :: String -> State B.ByteString Bool
takeOnly "" = return True
takeOnly (char:chars) = do
  next <- takeChar
  case next of
    Just char -> takeOnly chars
    _ -> return False

peekChar :: State B.ByteString (Maybe Char)
peekChar = state $ \input ->
  case C.uncons input of
    Nothing -> (Nothing, input)
    Just (char, chars) -> (Just char, input)

takeChar :: State B.ByteString (Maybe Char)
takeChar = state $ \input ->
  case C.uncons input of
    Nothing -> (Nothing, input)
    Just (char, chars) -> (Just char, chars)

isWhitespace :: Char -> Bool
isWhitespace char = case char of
                      ' ' -> True
                      '\n' -> True
                      '\t' -> True
                      '\r' -> True
                      _ -> False
