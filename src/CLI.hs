module CLI
  ( Arguments
  , ExitResult (..)
  , parseArgs
  , perform
  , putErrLn
  , showUsage
  ) where

import System.Environment
import System.Exit
import System.IO
import Data.Maybe
import Data.Version (showVersion)
import Paths_json_quick (version)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Prettify
import qualified Verify

type Command = String

type Option = String
type ShortOption = Char
type LongOptions = Map.Map Option OptionData
type ShortOptions = Map.Map ShortOption Option
data CommandOptions = CommandOptions
  { long :: LongOptions
  , short :: ShortOptions
  }
data OptionData = OptionData
  { optionType :: OptionType
  , optionHelp :: (String, String)
  }
data OptionType = Valued | Valueless deriving (Enum)

type Arguments = Map.Map Option String

data ExitResult = ExitValid | ExitInvalidUsage String | ExitInvalid String

type CommandAction = Handle -> Handle -> Arguments -> IO ExitResult

data GlobalOptionsData = GlobalOptionsData
  { inFileName :: Maybe String
  , outFileName :: Maybe String
  }

parseArgs :: [String] -> Either String (Command, Arguments)
parseArgs allArgs = do
  (command, args) <- separate allArgs
  parsedArgs <- parse args command
  return (command, parsedArgs)
    where separate (command:args) = Right (command, args)
          separate _ = Left "Expected a command, but no command was given."
          parse args command = Map.fromList <$> parseArgs' args command

parseArgs' :: [String] -> Command -> Either String [(Option, String)]
parseArgs' (('-':'-':longOption):remaining) command = do
  theOptionType <- (case (optionType <$> getLong) of
                     Just anOptionType -> Right anOptionType
                     Nothing -> Left $ "Option --" ++ longOption ++
                      " not found.")
  case theOptionType of
    Valued -> case remaining of
                (value:remaining) -> addArg longOption value remaining command
                _ -> Left $ "Expected an argument for option --" ++
                      longOption ++ " but no argument was given."
    Valueless -> addArg longOption "" remaining command
  where getLong = global longOption `fallbackTo` local command longOption
        global option = Map.lookup option $ long globalOptions
        local command option = do
          (_, localOptions) <- Map.lookup command commands
          Map.lookup option $ long localOptions

parseArgs' (('-':shortOption:[]):remaining) command = do
  longOption <- case getLong of
                  Just option -> Right option
                  Nothing -> Left  $"Option -" ++ shortOption:" not found."
  parseArgs' (('-':'-':longOption):remaining) command
  where getLong = global shortOption `fallbackTo` local command shortOption
        global option = Map.lookup option $ short globalOptions
        local command option = do
          (_, localOptions) <- Map.lookup command commands
          Map.lookup option $ short localOptions

parseArgs' (arg:_) _ = Left $ "Unexpected argument " ++ arg ++ "."
parseArgs' _ _ = Right []


addArg :: Option -> String -> [String] -> String ->
  Either String [(Option, String)]
addArg option value remaining command =
  add value <$> parseArgs' remaining command
  where add value = ((option, value):)

perform :: Command -> Arguments -> IO ExitResult
perform command args = do
  case Map.lookup command commands of
    Just (commandAction, _) -> do
      let globalData = getGlobalOptionsData args
      hIn <- maybe (return stdin) (flip openFile ReadMode) $
        inFileName globalData
      hOut <- maybe (return stdout) (flip openFile WriteMode) $
        outFileName globalData
      actionResult <- commandAction hIn hOut args
      hClose hIn
      hClose hOut
      return actionResult
    Nothing -> do
      return $ ExitInvalidUsage $ "Command " ++ command ++ " not found."

globalOptions :: CommandOptions
globalOptions = CommandOptions
  { long = Map.fromList
      [ ("in" , OptionData
          { optionType = Valued
          , optionHelp = ("file", "Read input from <file> instead of stdin.")
          })
      , ("out", OptionData
          { optionType = Valued
          , optionHelp = ("file", "Write output to <file> instead of stdout.")
          })
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
  , ("verify", (verify, verifyOptions))
  , ("help", (help, helpOptions))
  ]

prettify :: CommandAction
prettify inHandle outHandle _ = do
  contents <- B.hGetContents inHandle
  let prettified = Prettify.prettify contents
  B.hPut outHandle prettified
  return ExitValid

prettifyOptions :: CommandOptions
prettifyOptions = CommandOptions
  { long = Map.empty
  , short = Map.empty
  }

verify :: CommandAction
verify inHandle outHandle _ = do
  contents <- B.hGetContents inHandle
  let isValid = Verify.verify contents
  if isValid
     then do
       hPutStrLn outHandle "Valid JSON"
       return ExitValid
     else return $ ExitInvalid "Invalid JSON"

verifyOptions :: CommandOptions
verifyOptions = CommandOptions
  { long = Map.empty
  , short = Map.empty
  }

help :: CommandAction
help _ outHandle args = do
  let commandToHelpWith = Map.lookup "with" args
  case commandToHelpWith of
    Just command -> do
      let helpResult = helpTextFor command
      case helpResult of
        Right text -> do
          hPutStr outHandle text
          return ExitValid
        Left err -> do
          return $ ExitInvalidUsage $ err
    Nothing -> do
      hPutStr outHandle globalHelpText
      return ExitValid

globalHelpText :: String
globalHelpText = versionInformation ++
                 "\nAvailable commands:\n" ++ commandsInformation ++
                 "\nOptions:\n" ++
                 optionsInformation globalOptions ++
                 optionsInformation helpOptions

helpTextFor :: Command -> Either String String
helpTextFor command =
  case maybeLocalOptions of
    Just localOptions -> Right $ text localOptions
    Nothing -> Left $ "Cannot get help for unrecognized command " ++ command ++
                      "."
  where text localOptions = versionInformation ++ "\nOptions:\n" ++
                            optionsInformation globalOptions ++
                            optionsInformation localOptions
        maybeLocalOptions = snd <$> Map.lookup command commands

versionInformation :: String
versionInformation = "json-quick v" ++ showVersion version ++ ".\n\
                     \Originally written by Thomas Smith.\n\
                     \License: MIT <https://mit-license.org>.\n\
                     \Repo: <https://github.com/thomasebsmith/json-quick>.\n"

commandsInformation :: String
commandsInformation =  Map.foldrWithKey acc "" commands
  where acc name _ string = "\t" ++ name ++ "\n" ++ string

optionsInformation :: CommandOptions -> String
optionsInformation options = Map.foldrWithKey acc "" $ long options
  where acc name option string = "\t" ++ describe name option ++ "\n" ++ string

describe :: Option -> OptionData -> String
describe name option = optionUsage ++ ": " ++ description
  where (argument, description) = optionHelp option
        optionUsage = case optionType option of
                         Valued -> "--" ++ name ++ " <" ++ argument ++ ">"
                         Valueless -> "--" ++ name

helpOptions :: CommandOptions
helpOptions = CommandOptions
  { long = Map.fromList
    [ ("with", OptionData
        { optionType = Valued
        , optionHelp = ("command", "Get help for <command>.")
        })
    ]
  , short = Map.fromList
    [ ('w', "with")
    ]
  }

usage :: CommandOptions -> String
usage options = Map.foldrWithKey accumulator "" $ long options
  where accumulator name option string = " " ++ thisOptionUsage ++ string
          where thisOptionUsage = optionUsage name option

optionUsage :: String -> OptionData -> String
optionUsage name option = case optionType option of
                            Valued -> "[--" ++ name ++ " <" ++ argument ++ ">]"
                            Valueless -> "[--" ++ name ++ "]"
  where (argument, _) = optionHelp option

showUsage :: IO ()
showUsage = do
  progName <- getProgName
  putErrLn $ "Usage: " ++ progName ++ " <command>" ++ usage globalOptions
  putErrLn $ "Type \"" ++ progName ++ " help\" for more options."
  exitFailure

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

fallbackTo :: Maybe a -> Maybe a -> Maybe a
firstChoice `fallbackTo` secondChoice = case firstChoice of
                                          Just _ -> firstChoice
                                          Nothing -> secondChoice
