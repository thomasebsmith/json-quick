module CLI
  ( Arguments
  , parseArgs
  , perform
  , showUsage
  ) where

import System.Environment
import System.IO
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Prettify as Prettify

type Command = String

type Option = String
type ShortOption = Char
type LongOptions = Map.Map Option OptionType
type ShortOptions = Map.Map ShortOption Option
data CommandOptions = CommandOptions
  { long :: LongOptions
  , short :: ShortOptions
  }
data OptionType = Valued | Valueless deriving (Enum)

type Arguments = Map.Map Option String

type CommandAction = Handle -> Handle -> Arguments -> IO ()

data GlobalOptionsData = GlobalOptionsData
  { inFileName :: Maybe String
  , outFileName :: Maybe String
  }

parseArgs :: [String] -> Maybe (Command, Arguments)
parseArgs allArgs = do
  (command, args) <- separate allArgs
  parsedArgs <- parse args command
  return (command, parsedArgs)
    where separate (command:args) = Just (command, args)
          separate _ = Nothing
          parse args command = Map.fromList <$> parseArgs' args command

parseArgs' :: [String] -> Command -> Maybe [(Option, String)]
parseArgs' (('-':'-':longOption):remaining) command = do
  optionType <- getLong
  case optionType of
    Valued -> case remaining of
                (value:remaining) -> addArg longOption value remaining command
                _ -> Nothing
    Valueless -> addArg longOption "" remaining command
  where getLong = global longOption `fallbackTo` local command longOption
        global option = Map.lookup option $ long globalOptions
        local command option = do
          (_, localOptions) <- Map.lookup command commands
          Map.lookup option $ long localOptions

parseArgs' (('-':shortOption:[]):remaining) command = do
  longOption <- global shortOption `fallbackTo` local command shortOption
  parseArgs' (('-':'-':longOption):remaining) command
  where global option = Map.lookup option $ short globalOptions
        local command option = do
          (_, localOptions) <- Map.lookup command commands
          Map.lookup option $ short localOptions

parseArgs' [] _ = Just []

parseArgs' _ _ = Nothing

addArg :: Option -> String -> [String] -> String -> Maybe [(Option, String)]
addArg option value remaining command =
  add value <$> parseArgs' remaining command
  where add value = ((option, value):)

perform :: Command -> Arguments -> IO ()
perform command args = do
  case Map.lookup command commands of
    Just (commandAction, _) -> do
      let globalData = getGlobalOptionsData args
      hIn <- maybe (return stdin) (flip openFile ReadMode) $
        inFileName globalData
      hOut <- maybe (return stdout) (flip openFile WriteMode) $
        outFileName globalData
      commandAction hIn hOut args
      hClose hIn
      hClose hOut
    Nothing -> do
      showUsage

globalOptions :: CommandOptions
globalOptions = CommandOptions
  { long = Map.fromList
      [ ("in",  Valued)
      , ("out", Valued)
      ]
  , short = Map.fromList
      [ ('i', "in")
      , ('o', "out")
      ]
  }

getGlobalOptionsData :: Arguments -> GlobalOptionsData
getGlobalOptionsData args = GlobalOptionsData
  { inFileName = Map.lookup "in" args
  , outFileName = Map.lookup "out" args
  }

commands :: Map.Map Command (CommandAction, CommandOptions)
commands = Map.fromList
  [ ("prettify", (prettify, prettifyOptions))
  ]

prettify :: CommandAction
prettify inHandle outHandle _ = do
  contents <- B.hGetContents inHandle
  let prettified = Prettify.prettify contents
  B.hPut outHandle prettified

prettifyOptions :: CommandOptions
prettifyOptions = CommandOptions
  { long = Map.empty
  , short = Map.empty
  }


showUsage :: IO ()
showUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <command> <arguments...>"

fallbackTo :: Maybe a -> Maybe a -> Maybe a
firstChoice `fallbackTo` secondChoice = case firstChoice of
                                          Just _ -> firstChoice
                                          Nothing -> secondChoice
