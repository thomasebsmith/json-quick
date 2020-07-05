module Main where

import System.Environment
import CLI

main :: IO ()
main = do
  parsedArgs <- parseArgs <$> getArgs
  case parsedArgs of
    Just (command, args) -> do
      perform command args
    Nothing -> do
      showUsage
      return ()
