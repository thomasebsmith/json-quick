module Parse
( parse
, succeeded
, JSONValue (..)
, ParseError
) where

import Prelude hiding (takeWhile)

import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified FailList as F

type ParseError = String
type ParseResult = (Either ParseError JSONValue, B.ByteString)
type JSONPair = (C.ByteString, JSONValue)
data JSONValue = JSONObject (F.FailList ParseError JSONPair)
               | JSONArray (F.FailList ParseError JSONValue)
               | JSONNumber C.ByteString
               | JSONString C.ByteString
               | JSONTrue
               | JSONFalse | JSONNull

parse :: B.ByteString -> ParseResult
parse = runState takeValue

succeeded :: ParseResult -> Maybe ParseError
succeeded result = case result of
                     (Right value, rest) ->
                       let nonWhitespace = snd $ runState takeWhitespace rest in
                           if B.null nonWhitespace
                              then getError value
                              else Just $
                                "Unexpected tokens after end of JSON value (" ++
                                firstFew nonWhitespace ++ ")"
                     (Left err, _) -> Just err

firstFew :: B.ByteString -> String
firstFew string = takeUpTo 500 string ++ "..."

takeUpTo :: Int -> B.ByteString -> String
takeUpTo n str
  | n <= 0 = ""
  | otherwise = case C.uncons str of
                  Nothing -> ""
                  Just (char, chars) -> char:takeUpTo (n - 1) chars

getError :: JSONValue -> Maybe ParseError
getError (JSONObject list) = getErrorFromList $ snd <$> list
getError (JSONArray list) = getErrorFromList list
getError _ = Nothing

getErrorFromList :: F.FailList ParseError JSONValue -> Maybe ParseError
getErrorFromList list = case F.foldr (\_ acc -> acc) () list of
                     Left err -> Just err
                     _ -> Nothing

takeValue :: State B.ByteString (Either ParseError JSONValue)
takeValue = do
  takeWhitespace
  next <- peekChar
  case next of
    Nothing -> return $ Left "Unexpected EOF while reading JSON value"
    Just char
      | char == '{' -> takeObject
      | char == '[' -> takeArray
      | char == '-' -> takeNegativeNumber
      | isDigit char -> takeNumber
      | char == '"' -> takeString
      | char == 't' -> takeTrue
      | char == 'f' -> takeFalse
      | char == 'n' -> takeNull
      | otherwise -> return $ Left $ "Unexpected character " ++ show char ++
                                     " while reading JSON value"

takeTrue :: State B.ByteString (Either ParseError JSONValue)
takeTrue = do
  succeeded <- takeOnly "true"
  if succeeded
     then return $ Right JSONTrue
     else return $ Left "Invalid literal beginning with 't'."

takeFalse :: State B.ByteString (Either ParseError JSONValue)
takeFalse = do
  succeeded <- takeOnly "false"
  if succeeded
     then return $ Right JSONFalse
     else return $ Left "Invalid literal beginning with 'f'."

takeNull :: State B.ByteString (Either ParseError JSONValue)
takeNull = do
  succeeded <- takeOnly "null"
  if succeeded
     then return $ Right JSONNull
     else return $ Left "Invalid literal beginning with 'n'."

takeNegativeNumber :: State B.ByteString (Either ParseError JSONValue)
takeNegativeNumber = do
  takeChar -- the minus sign
  num <- takeNumber
  case num of
    Right (JSONNumber str) -> return $ Right $ JSONNumber $ '-' `C.cons` str
    err -> return err

takeNumber :: State B.ByteString (Either ParseError JSONValue)
takeNumber = do
  maybeDigit <- peekChar
  case maybeDigit of
    Nothing -> return $ Left "Unexpected EOF while reading number"
    Just '0' -> do
      takeChar
      decimal <- takeDecimal
      return $ JSONNumber <$> ('0' `C.cons`) <$> decimal
    Just char -> do
      num <- takeNumber'
      return $ JSONNumber <$> num

takeNumber' :: State B.ByteString (Either ParseError C.ByteString)
takeNumber' = do
  maybeDigit <- peekChar
  case maybeDigit of
    Nothing -> return $ Right C.empty
    Just char
      | isDigit char -> do
          takeChar
          remaining <- takeNumber'
          return $ (char `C.cons`) <$> remaining
      | otherwise -> takeDecimal

takeDecimal :: State B.ByteString (Either ParseError C.ByteString)
takeDecimal = do
  maybeDot <- peekChar
  case maybeDot of
    Nothing -> return $ Right C.empty
    Just '.' -> do
      takeChar
      decimal <- takeDecimal'
      return $ ('.' `C.cons`) <$> decimal
    _ -> takeExponent

takeDecimal' :: State B.ByteString (Either ParseError C.ByteString)
takeDecimal' = do
  maybeDigit <- takeChar
  case maybeDigit of
    Nothing -> return $ Left "Unexpected EOF while reading number"
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
            Nothing -> return $ Right $ C.singleton digit
      | otherwise -> return $ Left $ "Unexpected character " ++
                                     show digit ++ " while reading number"

takeExponent :: State B.ByteString (Either ParseError C.ByteString)
takeExponent = do
  maybeE <- peekChar
  case maybeE of
    Nothing -> return $ Right $ C.empty
    Just char
      | char == 'e' || char == 'E' -> do
          takeChar
          exponent <- takeExponent'
          return $ (char `C.cons`) <$> exponent
      | otherwise -> return $ Right $ C.empty

takeExponent' :: State B.ByteString (Either ParseError C.ByteString)
takeExponent' = do
  maybeSign <- peekChar
  case maybeSign of
    Nothing -> return $ Left "Unexpected EOF while reading number"
    Just sign
      | sign == '+' || sign == '-' -> do
          takeChar
          exponent <- takeExponent''
          return $ (sign `C.cons`) <$> exponent
      | otherwise -> takeExponent''

takeExponent'' :: State B.ByteString (Either ParseError C.ByteString)
takeExponent'' = do
  maybeDigit <- takeChar
  case maybeDigit of
    Nothing -> return $ Left "Unexpected EOF while reading number"
    Just digit
      | isDigit digit -> do
          digits <- takeWhile isDigit
          return $ Right $ digit `C.cons` digits
      | otherwise -> return $ Left $ "Unexpected character " ++ show digit ++
                                     " while reading number"

-- Take an array from input. Requires that an opening [ has been detected.
takeArray :: State B.ByteString (Either ParseError JSONValue)
takeArray = Right <$> (JSONArray <$> takeArray' True)

takeArray' :: Bool -> State B.ByteString (F.FailList ParseError JSONValue)
takeArray' isFirst = do
  takeWhitespace
  maybeChar <- takeChar
  case maybeChar of
    Nothing -> return $ F.Fail "Unexpected EOF while reading array element"
    Just char
      | char == ']' -> return F.Empty
      | isFirst || char == ',' -> do
          takeWhitespace
          next <- peekChar
          if isFirst && next == Just ']'
             then do
               takeChar
               return F.Empty
             else do
               maybeValue <- takeValue
               case maybeValue of
                 Left err -> return $ F.Fail err
                 Right value -> do
                   remaining <- takeArray' False
                   return $ F.Cons value remaining
      | otherwise -> return $ F.Fail $ "Unexpected character " ++ show char ++
                                       " while reading array element"

-- Take a string from input. Requires that an opening " has been detected.
takeString :: State B.ByteString (Either ParseError JSONValue)
takeString = do
  takeChar
  maybeString <- takeString'
  return $ JSONString <$> maybeString

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
                      then do
                        maybeEscaped <- takeChar
                        case maybeEscaped of
                          Nothing -> return $ Left "Unexpected EOF while\
                                                   \ reading string escape"
                          Just escaped -> return $ Right $
                            escape `C.cons` C.singleton escaped
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
isSingleEscapeChar '_' = False

-- Takes an object from input. Requires that an initial { has been detected.
takeObject :: State B.ByteString (Either ParseError JSONValue)
takeObject = Right <$> JSONObject <$> takeObject' True

takeObject' :: Bool -> State B.ByteString (F.FailList ParseError JSONPair)
takeObject' isFirst = do
  takeWhitespace
  maybeChar <- takeChar
  case maybeChar of
    Nothing -> return $ F.Fail "Unexpected EOF while reading object"
    Just char
      | char == '}' -> return F.Empty
      | (char == '{' && isFirst) || char == ',' -> do
          takeWhitespace
          nextChar <- takeChar
          case nextChar of
            Just '"' -> do
              maybeProp <- takeProperty
              case maybeProp of
                Right prop -> do
                  remaining <- takeObject' False
                  return $ F.Cons prop remaining
                Left err -> return $ F.Fail err
            Just otherChar -> return $ F.Fail $
              "Unexpected " ++ show otherChar ++ " while reading object key"
            Nothing -> return $ F.Fail "Unexpected EOF while reading object key"
      | otherwise -> return $ F.Fail $
          "Unexpected character " ++ show char ++ " while reading object"

takeProperty :: State B.ByteString (Either ParseError (C.ByteString, JSONValue))
takeProperty = do
  maybeKey <- takeString'
  takeWhitespace
  maybeColon <- takeChar
  takeWhitespace
  case (maybeKey, maybeColon) of
    (Right key, Just ':') -> do
      maybeValue <- takeValue
      case maybeValue of
        Left err -> return $ Left err
        Right value -> do
          return $ Right (key, value)
    (Left err, _) -> return $ Left err
    (_, Just char) -> return $ Left $
      "Unexpected " ++ show char ++
      " while reading object property (expected ':')"
    (_, Nothing) -> return $ Left "Unexpected EOF while reading object property"

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
                          takeChar
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
