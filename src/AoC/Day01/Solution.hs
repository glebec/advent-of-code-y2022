module AoC.Day01.Solution where

import AoC.Day01.Input (input)
import AoC.Solver ((:->:)(..))

import Data.List.Split (splitOn)

takeInventories :: String -> [[Int]]
takeInventories = (fmap . fmap) read . splitOn [""] . lines

inventories :: [[Int]]
inventories = read <$> lines input

-- part 1
part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

-- part 2
-- TODO

-- Work in progress towards making this repo more of a framework:

day01a :: [[Int]] :->: Int
day01a = MkSol
    { sParse = Just . takeInventories
    , sSolve = Just . part1
    , sPrint = show
    }

-- day01b :: TODO
