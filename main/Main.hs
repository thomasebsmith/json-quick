module Main where

import System.Environment
import System.Exit
import CLI

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

showBadUsage :: String -> IO ()
showBadUsage msg = do
  putErrLn msg
  showUsage
  exitFailure

showError :: String -> IO ()
showError msg = do
  putErrLn msg
  exitFailure
