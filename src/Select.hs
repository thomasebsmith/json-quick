module Select
( parseAndSelect
, parseSelection
, select
, Selection
) where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Parse
import ParseUtilities
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified FailList as F

data Selection = SelectThis | Property B.ByteString Selection

parseAndSelect :: B.ByteString -> B.ByteString -> Either ParseError JSONValue
parseAndSelect toParse toSelect = result
  where result = join $ select <$> parseSelection toSelect <*> json
        json = case parse toParse of
                 (Right value, rest) ->
                   let nonWhitespace = snd $ runState takeWhitespace rest in
                       if B.null nonWhitespace
                         then Right value
                         else Left "Unexpected tokens after end of JSON value"
                 (Left err, _) -> Left err

parseSelection :: B.ByteString -> Either ParseError Selection
parseSelection = fst . runState parseSelection'

parseSelection' :: State B.ByteString (Either ParseError Selection)
parseSelection' = do
  takeWhitespace
  maybeNext <- peekChar
  case maybeNext of
    Nothing -> return $ Right SelectThis
    Just '"' -> do
      maybeString <- takeString
      takeWhitespace
      maybeDot <- takeChar
      maybeRemaining <- parseSelection'
      case (maybeString, maybeDot, maybeRemaining) of
        (Left err, _, _) -> return $ Left err
        (_, _, Left err) -> return $ Left err
        (Right string, Just '.', Right selection) -> return . Right $
          Property string selection
        (Right string, Nothing, Right selection) -> return . Right $
          Property string selection
        (_, Just other, _) -> return . Left $
          "Expected '.' but found " ++ show other
    Just otherChar -> return . Left $ "Unexpected character " ++ show otherChar

select :: Selection -> JSONValue -> Either ParseError JSONValue
select SelectThis value = Right value

select _ (JSONObject (F.Fail err)) = Left err
select _ (JSONArray (F.Fail err)) = Left err

select (Property key selection) (JSONObject props) =
  case maybeProp of
    Just prop -> select selection $ snd prop
    Nothing -> Left $ "Property " ++ show key ++ " not found in object"
  where maybeProp = F.head $ F.filter ((== key) . fst) props

select (Property key selection) (JSONArray values) =
  case maybeIndex of
    Just index -> case F.atIndex index values of
                    Just value -> select selection value
                    Nothing -> Left $
                      "Index " ++ show index ++ " not found in array"
    Nothing -> Left $ "Key " ++ show key ++ " is not a valid array index"
  where maybeIndex = case C.readInt key of
                        Just (int, remaining) -> if remaining == B.empty
                                                  then Just int
                                                  else Nothing
                        Nothing -> Nothing

select (Property key _) _ = Left $
  "Cannot select property " ++ show key ++
  " from non-object and non-array value"
