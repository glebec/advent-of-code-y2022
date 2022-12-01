module AoC.Day01.Solution where

import AoC.Solver ((:->:)(..))

import Data.List (sortBy)
import Data.List.Split (splitOn)

takeInventories :: String -> [[Int]]
takeInventories = (fmap . fmap) read . splitOn [""] . lines

-- part 1
biggestInventory :: [[Int]] -> Int
biggestInventory = maximum . fmap sum

-- part 2
topThreeInventories :: [[Int]] -> Int
topThreeInventories = sum . take 3 . sortBy (flip compare) . fmap sum

-- Work in progress towards making this repo more of a framework:

day01a :: [[Int]] :->: Int
day01a = MkSol
    { sParse = Just . takeInventories
    , sSolve = Just . biggestInventory
    , sPrint = show
    }

-- day01b :: TODO
day01b :: [[Int]] :->: Int
day01b = MkSol
    { sParse = Just . takeInventories
    , sSolve = Just . topThreeInventories
    , sPrint = show
    }
