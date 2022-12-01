module AoC.Day01.Solution where

import AoC.Solver ((:->:)(..))

import Data.List (sortBy)
import Data.List.Split (splitOn)

takeInventories :: String -> [[Int]]
takeInventories = (fmap . fmap) read . splitOn [""] . lines

day01a :: [[Int]] :->: Int
day01a = MkSol
    { sParse = Just . takeInventories
    , sSolve = Just . maximum . fmap sum
    , sPrint = show
    }

day01b :: [[Int]] :->: Int
day01b = MkSol
    { sParse = Just . takeInventories
    , sSolve = Just . sum . take 3 . sortBy (flip compare) . fmap sum
    , sPrint = show
    }
