module Main where

import System.Environment
import System.Exit
import CLI

-- | 'main' is the entry point for the json-quick command line interface.
-- For more details about argument parsing, see src/CLI.hs.
main :: IO ()
main = do
  parsedArgs <- parseArgs <$> getArgs
  case parsedArgs of
    Right (command, args) -> do
      exitResult <- perform command args
      case exitResult of
        ExitInvalidUsage errorMsg -> showBadUsage errorMsg
        ExitInvalid errorMsg -> showError errorMsg
        ExitValid -> return ()
    Left errorMsg -> do
      showBadUsage errorMsg

-- | The 'showBadUsage' function shows an error (msg) and then shows general
-- usage and help information. It then causes the program to exit with a
-- non-zero exit status.
showBadUsage :: String -> IO ()
showBadUsage msg = do
  putErrLn msg
  showUsage
  exitFailure

-- | The 'showError' function shows an error (msg) and then causes the program
-- to exit with a non-zero exit status. This is used instead of 'showBadUsage'
-- when the general syntax of an invocation was correct, but it produced an
-- error.
showError :: String -> IO ()
showError msg = do
  putErrLn msg
  exitFailure
