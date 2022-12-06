module AoC.Day06.Solution where

import AoC.Solver ((:->:)(..))
import Data.List (findIndex, nub, zipWith4)

processA :: String -> Maybe Int
processA str = (+4) <$> findIndex ((== 4) . length . nub) quads where
    quads = zipWith4 (\a b c d -> [a, b, c, d])
            str (drop 1 str) (drop 2 str) (drop 3 str)

day06a :: String :->: Int
day06a = MkSol
    { sParse = Just
    , sSolve = processA
    , sPrint = show
    }
