module Main where

import System.Environment (getArgs, getProgName)
import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    path:_ -> solveFile path

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ progName ++ " filename"

solveFile :: FilePath -> IO ()
solveFile path = do
  file <- readFile path
  if isValidSource file
    then do
      case solve (parseBoard file) of
        [] -> putStrLn "Cannot solve."
        board:_ -> putStrLn $ showBoard board
    else do
      putStrLn "Invalid source file."
