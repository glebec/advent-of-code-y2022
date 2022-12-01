module AoC.Day01.Solution where

import AoC.Day01.Input (input)
import AoC.Solver ((:->:)(..))

exampleNums :: [Int]
exampleNums = read <$> lines input

-- part 1
examplePart1 :: [Int] -> Int
examplePart1 = sum

-- part 2
-- TODO

-- Work in progress towards making this repo more of a framework:

day01a :: [Int] :->: Int
day01a = MkSol
    { sParse = Just . fmap read . lines
    , sSolve = Just . examplePart1
    , sPrint = show
    }

-- day01b :: TODO
