module Parse
( parse
, showAsByteString
, succeeded
, JSONValue (..)
, ParseError
) where

import Prelude hiding (takeWhile)
import Control.Monad.Trans.State.Lazy
import Data.Char
import ParseUtilities
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified FailList as F

type ParseResult = (Either ParseError JSONValue, B.ByteString)
type JSONPair = (C.ByteString, JSONValue)
data JSONValue = JSONObject (F.FailList ParseError JSONPair)
               | JSONArray (F.FailList ParseError JSONValue)
               | JSONNumber C.ByteString
               | JSONString C.ByteString
               | JSONTrue
               | JSONFalse
               | JSONNull

type JSONOutputOptions = ()

defaultOptions :: JSONOutputOptions
defaultOptions = ()

showAsByteString :: JSONValue -> Maybe B.ByteString
showAsByteString input = (`C.append` C.singleton '\n') <$> shown
  where shown = showAsByteString' defaultOptions input

showAsByteString' :: JSONOutputOptions -> JSONValue -> Maybe C.ByteString
showAsByteString' opts (JSONObject props) =
  case showFailList $ showProp <$> props of
    Just text -> Just $ ('{' `C.cons` text) `C.append` C.singleton '}'
    Nothing -> Nothing
  where showProp (key, val) = C.append <$> showKey key <*> showValue val
        showKey key = showAsByteString' opts $ JSONString key
        showValue val = C.cons ':' <$> showAsByteString' opts val
showAsByteString' opts (JSONArray list) =
  case showFailList $ showAsByteString' opts <$> list of
    Just listText -> Just $ ('[' `C.cons` listText) `C.append` C.singleton ']'
    Nothing -> Nothing
showAsByteString' _ (JSONNumber number) = Just number
showAsByteString' _ (JSONString string) =
  Just $ ('"' `C.cons` string) `C.append` C.singleton '"'
showAsByteString' _ JSONTrue = Just $ C.pack "true"
showAsByteString' _ JSONFalse = Just $ C.pack "false"
showAsByteString' _ JSONNull = Just $ C.pack "null"

showFailList :: F.FailList e (Maybe C.ByteString) -> Maybe C.ByteString
showFailList (F.Fail _) = Nothing
showFailList F.Empty = Just C.empty
showFailList (F.Cons (Just x) xs@(F.Cons _ _)) = C.append x <$>
                                                 C.cons ',' <$>
                                                 showFailList xs
showFailList (F.Cons Nothing _) = Nothing
showFailList (F.Cons x F.Empty) = x
showFailList (F.Cons _ (F.Fail _)) = Nothing

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
      | char == '"' -> do
          string <- takeString
          return $ JSONString <$> string
      | char == 't' -> takeTrue
      | char == 'f' -> takeFalse
      | char == 'n' -> takeNull
      | otherwise -> return $ Left $ "Unexpected character " ++ show char ++
                                     " while reading JSON value"

takeTrue :: State B.ByteString (Either ParseError JSONValue)
takeTrue = do
  didSucceed <- takeOnly "true"
  if didSucceed
     then return $ Right JSONTrue
     else return $ Left "Invalid literal beginning with 't'."

takeFalse :: State B.ByteString (Either ParseError JSONValue)
takeFalse = do
  didSucceed <- takeOnly "false"
  if didSucceed
     then return $ Right JSONFalse
     else return $ Left "Invalid literal beginning with 'f'."

takeNull :: State B.ByteString (Either ParseError JSONValue)
takeNull = do
  didSucceed <- takeOnly "null"
  if didSucceed
     then return $ Right JSONNull
     else return $ Left "Invalid literal beginning with 'n'."

takeNegativeNumber :: State B.ByteString (Either ParseError JSONValue)
takeNegativeNumber = do
  _ <- takeChar -- the minus sign
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
      _ <- takeChar
      decimal <- takeDecimal
      return $ JSONNumber <$> ('0' `C.cons`) <$> decimal
    Just _ -> do
      num <- takeNumber'
      return $ JSONNumber <$> num

takeNumber' :: State B.ByteString (Either ParseError C.ByteString)
takeNumber' = do
  maybeDigit <- peekChar
  case maybeDigit of
    Nothing -> return $ Right C.empty
    Just char
      | isDigit char -> do
          _ <- takeChar
          remaining <- takeNumber'
          return $ (char `C.cons`) <$> remaining
      | otherwise -> takeDecimal

takeDecimal :: State B.ByteString (Either ParseError C.ByteString)
takeDecimal = do
  maybeDot <- peekChar
  case maybeDot of
    Nothing -> return $ Right C.empty
    Just '.' -> do
      _ <- takeChar
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
                  theExponent <- takeExponent
                  return $ (digit `C.cons`) <$> theExponent
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
          _ <- takeChar
          theExponent <- takeExponent'
          return $ (char `C.cons`) <$> theExponent
      | otherwise -> return $ Right $ C.empty

takeExponent' :: State B.ByteString (Either ParseError C.ByteString)
takeExponent' = do
  maybeSign <- peekChar
  case maybeSign of
    Nothing -> return $ Left "Unexpected EOF while reading number"
    Just sign
      | sign == '+' || sign == '-' -> do
          _ <- takeChar
          theExponent <- takeExponent''
          return $ (sign `C.cons`) <$> theExponent
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
               _ <- takeChar
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
