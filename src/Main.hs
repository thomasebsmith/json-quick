module Main where

import System.Environment
import System.Exit
import CLI

main :: IO ()
main = do
  parsedArgs <- parseArgs <$> getArgs
  case parsedArgs of
    Right (command, args) -> do
      maybeError <- perform command args
      case maybeError of
        Just errorMsg -> do
          showError errorMsg
        Nothing -> do
          return ()
    Left errorMsg -> do
      showError errorMsg

showError msg = do
  putErrLn msg
  showUsage
  exitFailure
