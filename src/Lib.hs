module Lib {-
    ( someFunc
    , positions
    , parseNumber
    , Cell(..)
    , isConfirmed
    ) -} where

import Data.Array
import Data.List (sortOn, partition)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Cell = Confirmed Int -- 確定した数
          | Candidates [Int] -- 候補の数
            deriving (Show, Eq)

type Pos = (Int, Int)
type PosCell = (Pos, Cell)

data Board = Board
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
updateCell (Candidates xs) (Candidates ys) = Candidates $ foldr f xs ys
    where f y xs' = filter (/=y) xs'

candiatesCell :: PosCell -> [PosCell]
candiatesCell pc@(_, (Candidates _)) = [pc]
candiatesCell pc@((y, x), (Confirmed n)) = pc : row ++ col ++ sec
    where row = candiatesRow pc
          col = candiatesCol pc
          sec = candiatesSec pc

candiatesRow :: PosCell -> [PosCell]
candiatesRow pc@(_, (Candidates _)) = []
candiatesRow ((y, _), (Confirmed n)) = [((y, x), Candidates [n]) | x <- [1..9]]

candiatesCol :: PosCell -> [PosCell]
candiatesCol pc@(_, (Candidates _)) = []
candiatesCol ((_, x), (Confirmed n)) = [((y, x), Candidates [n]) | y <- [1..9]]

candiatesSec :: PosCell -> [PosCell]
candiatesSec pc@(_, (Candidates _)) = []
candiatesSec ((y, x), (Confirmed n)) = [((y' + dy, x' + dx), Candidates [n]) | dy <- [1..3], dx <- [1..3]]
    where base a = (a - 1) `div` 3 * 3
          x' = base x
          y' = base y

solve :: Board -> [Board]
solve b = solve' [b] []
    where solve' []          solved = solved
          solve' (x:solving) solved =
            case next of
                [] -> solve' solving solved
                c:_ -> let (ns, ms) = divideWithSolved c
                    in solve' (ms ++ solving) (ns ++ solved)
                where next = nextCandiates x
                      divideWithSolved pc = partition isSolved $ divide pc x

isSolved :: Board -> Bool
isSolved (Board b) = all isConfirmed $ elems b

divide :: PosCell -> Board -> [Board]
divide pc@(_, (Confirmed _)) b = undefined
divide pc@((y,x), (Candidates ns)) b = map (updateBoard b) pcs
    where pcs = [((y, x), Confirmed n) | n <- ns]

nextCandiates :: Board -> [PosCell]
nextCandiates (Board b) = sortOn (\(_, Candidates ns) -> length ns) cs
    where cs = filter (not . isConfirmed . snd) $ assocs b

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

sampleSrc :: [String]
sampleSrc = [ "5176   34" -- 1
            , "289  4   " -- 2
            , "3462 5 9 " -- 3
            , "6 2    1 " -- 4
            , " 38  6 47" -- 5
            , "         " -- 6
            , " 9     78" -- 7
            , "7034  56 " -- 8
            , "         " -- 9
            ]

sampleBoard :: Board
sampleBoard = parseBoard $ unlines sampleSrc

