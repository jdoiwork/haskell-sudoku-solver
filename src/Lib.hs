{-# LANGUAGE BangPatterns #-}

module Lib where

import Data.Array.IArray
import Data.List (sortOn, partition, minimumBy)

data Cell = Confirmed !Int -- 確定した数
          | Candidates [Int] -- 候補の数
            deriving (Show, Eq)

type Pos = (Int, Int)
type PosCell = (Pos, Cell)

newtype Board = Board
              { board :: (Array Pos Cell)
              } deriving Show

positions :: [Pos]
positions = do
    y <- [1..9]
    x <- [1..9]
    return (y, x)

parseBoard :: String -> Board
parseBoard s = foldr (flip updateBoard) emptyBoard cs
    where ns = map parseNumber $ concat $ lines s
          cs = zip positions ns

updateBoard :: Board -> PosCell -> Board
updateBoard (Board b) pc = Board $ accum updateCell b $ candiatesCell pc

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
updateCell (Candidates xs) (Candidates ys) = Candidates ns
    where f y xs' = filter (/=y) xs'
          !ns = foldr f xs ys

candiatesCell :: PosCell -> [PosCell]
candiatesCell pc@(_, (Candidates _)) = [pc]
candiatesCell pc@((y, x), (Confirmed n)) = pc : row ++ col ++ sec
    where row = candiatesRow pc
          col = candiatesCol pc
          sec = candiatesSec pc

candiatesRow :: PosCell -> [PosCell]
candiatesRow ((y, _), (Confirmed n)) = [((y, x), Candidates [n]) | x <- [1..9]]
candiatesRow pc@(_, (Candidates _)) = []

candiatesCol :: PosCell -> [PosCell]
candiatesCol ((_, x), (Confirmed n)) = [((y, x), Candidates [n]) | y <- [1..9]]
candiatesCol pc@(_, (Candidates _)) = []

candiatesSec :: PosCell -> [PosCell]
candiatesSec ((y, x), (Confirmed n)) = [((y' + dy, x' + dx), Candidates [n]) | dy <- [1..3], dx <- [1..3]]
    where base a = (a - 1) `div` 3 * 3
          x' = base x
          y' = base y
candiatesSec pc@(_, (Candidates _)) = []

solve :: Board -> [Board]
solve b = solve' [b] []
    where solve' []       solved              = solved
          solve' (x:rest) solved | isSolved x = [x] -- solve' rest (x : solved)
                                 | otherwise  = solve' ((nextBoards x) ++ rest) solved
          nextBoards x = concatMap (divide x) $ nextCandiate1 x

isSolved :: Board -> Bool
isSolved (Board b) = all isConfirmed $ elems b

divide :: Board -> PosCell -> [Board]
divide _ pc@(_, (Confirmed _)) = []
divide b pc@((y,x), (Candidates ns)) = map (updateBoard b) pcs
    where pcs = [((y, x), Confirmed n) | n <- ns]

nextCandiates :: Board -> [PosCell]
nextCandiates (Board b) = sortOn (\(_, Candidates ns) -> length ns) cs
    where cs = filter (not . isConfirmed . snd) $ assocs b

nextCandiate1 :: Board -> [PosCell]
nextCandiate1 (Board b) = case cs of
        [] -> []
        _ -> return $ minimumBy cmplen cs
    where cs = filter (not . isConfirmed . snd) $ assocs b
          cmplen (_, Candidates a) (_, Candidates b) = length a `compare` length b

take9s :: [a] -> [[a]]
take9s [] = []
take9s xs = take 9 xs : take9s (drop 9 xs)

showBoard :: Board -> String
showBoard (Board b) = unlines $ map showRow $ take9s $ elems b

showRow :: [Cell] -> String
showRow = map showCell

showCell :: Cell -> Char
showCell (Confirmed n) = head $ show n
showCell _             = ' '

isValidSource :: String -> Bool
isValidSource s = rowOK && all colOK rows
    where rows = take n $ lines s
          rowOK = n == length rows
          colOK r = n == length r
          n = 9
