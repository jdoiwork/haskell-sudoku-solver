module SampleData where

import Lib

sampleSrc :: String
sampleSrc = unlines
    [ "5176   34" -- 1
    , "289  4   " -- 2
    , "3462 5 9 " -- 3
    , "6 2    1 " -- 4
    , " 38  6 47" -- 5
    , "         " -- 6
    , " 9     78" -- 7
    , "7034  56 " -- 8
    , "         " -- 9
    ]

sampleSrc2 :: String
sampleSrc2 = unlines
    [ "850002400" -- 1
    , "720000009" -- 2
    , "004000000" -- 3
    , "000107002" -- 4
    , "305000900" -- 5
    , "040000000" -- 6
    , "000080070" -- 7
    , "017000000" -- 8
    , "000036040" -- 9
    ]

sampleSrc3 :: String
sampleSrc3 = unlines
    [ "370001000" -- 1
    , "000902000" -- 2
    , "000006510" -- 3
    , "002000640" -- 4
    , "190000000" -- 5
    , "040009020" -- 6
    , "004000150" -- 7
    , "000800000" -- 8
    , "600500700" -- 9
    ]

sampleBoard :: Board
sampleBoard = parseBoard sampleSrc

sampleBoard2 :: Board
sampleBoard2 = parseBoard sampleSrc2

sampleBoard3 :: Board
sampleBoard3 = parseBoard sampleSrc3
