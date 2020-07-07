module CLI
  ( Arguments
  , parseArgs
  , perform
  , putErrLn
  , showUsage
  ) where

import System.Environment
import System.Exit
import System.IO
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import qualified Prettify as Prettify

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

type CommandAction = Handle -> Handle -> Arguments -> IO ()

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
      putErrLn $ "Command " ++ command ++ " not found."
      showUsage

globalOptions :: CommandOptions
globalOptions = CommandOptions
  { long = Map.fromList
      [ ("in" , OptionData
          { optionType = Valued
          , optionHelp = ("file", "Read input from *file* instead of stdin.")
          })
      , ("out", OptionData
          { optionType = Valued
          , optionHelp = ("file", "Write output to *file* instead of stdout.")
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
  putErrLn $ "Type \"" ++ progName ++ " --help\" for more options."
  exitFailure

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

fallbackTo :: Maybe a -> Maybe a -> Maybe a
firstChoice `fallbackTo` secondChoice = case firstChoice of
                                          Just _ -> firstChoice
                                          Nothing -> secondChoice
