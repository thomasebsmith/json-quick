module Main where

import System.Environment
import CLI

main :: IO ()
main = do
  parsedArgs <- parseArgs <$> getArgs
  case parsedArgs of
    Right (command, args) -> do
      perform command args
    Left errorMsg -> do
      putErrLn errorMsg
      showUsage
      return ()
