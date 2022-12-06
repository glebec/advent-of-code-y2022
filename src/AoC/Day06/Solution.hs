module AoC.Day06.Solution where

import AoC.Solver ((:->:)(..))
import Data.Containers.ListUtils (nubIntOn)

findUniq :: Int -> String -> Maybe Int
findUniq n str = go n str where
    go _ [] = Nothing
    go i rest | n == length (nubIntOn fromEnum $ take n rest) = Just i
              | otherwise = go (i + 1) (tail rest)

day06a, day06b :: String :->: Int
day06a = MkSol {sParse = Just, sSolve = findUniq 4, sPrint = show}
day06b = day06a {sSolve = findUniq 14}
