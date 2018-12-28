module Lib
    ( someFunc
    , positions
    , parseNumber
    , Cell(..)
    , isConfirmed
    ) where

import Data.Array

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Cell = Confirmed Int -- 確定した数
          | Candidates [Int] -- 候補の数
            deriving (Show, Eq)

type Pos = (Int, Int)

data Board = Board (Array Pos Cell) deriving Show

positions :: [Pos]
positions = do
    y <- [1..9]
    x <- [1..9]
    return (y, x)

parseBoard :: String -> Board
parseBoard s = undefined

parseNumber :: Char -> Cell
parseNumber c | '1' <= c && c <= '9' = Confirmed $ read $ c:[]
              | otherwise            = Candidates [1..9]

isConfirmed :: Cell -> Bool
isConfirmed (Confirmed _) = True
isConfirmed _ = False

emptyCell :: Cell
emptyCell = Candidates [1..9]
