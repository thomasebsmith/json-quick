module Main where

import System.Environment
import System.IO
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Prettify as Prettify

type Arguments = [String]
type Command = String
type CommandAction = Handle -> Handle -> [String] -> IO ()

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Just (command, args) -> do
      perform command args
    Nothing -> do
      showUsage
      return ()

parseArgs :: [String] -> Maybe (Command, Arguments)
parseArgs (command:args) = Just (command, args)
parseArgs _ = Nothing

perform :: Command -> Arguments -> IO ()
perform command args = do
  case Map.lookup command commands of
    Just commandAction -> do
      commandAction stdin stdout args
    Nothing -> do
      showUsage

commands :: Map.Map Command CommandAction
commands = Map.fromList
  [
    ("prettify", prettify)
  ]

prettify :: CommandAction
prettify inHandle outHandle _ = do
  contents <- B.hGetContents inHandle
  let prettified = Prettify.prettify contents
  B.hPut outHandle prettified

showUsage :: IO ()
showUsage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <command> <arguments...>"
