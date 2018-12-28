module Lib {-
    ( someFunc
    , positions
    , parseNumber
    , Cell(..)
    , isConfirmed
    ) -} where

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
parseBoard s = emptyBoard

parseNumber :: Char -> Cell
parseNumber c | '1' <= c && c <= '9' = Confirmed $ read $ c:[]
              | otherwise            = Candidates []

isConfirmed :: Cell -> Bool
isConfirmed (Confirmed _) = True
isConfirmed _ = False

emptyCell :: Cell
emptyCell = Candidates [1..9]

emptyBoard :: Board
emptyBoard = Board $ array ((1,1), (9,9)) $ zip positions $ repeat emptyCell

updateCell :: Cell -> Cell -> Cell
updateCell _ b@(Confirmed _) = b
updateCell a@(Confirmed _) _ = a
updateCell (Candidates xs) (Candidates ys) = Candidates $ foldr f xs ys
    where f y xs' = filter (/=y) xs'
