module AoC.Day06.Solution where

import AoC.Solver ((:->:)(..))
import Data.Set qualified as S

findSet :: Int -> String -> Maybe Int
findSet n str = go n str where
    go _ [] = Nothing
    go i rest | n == S.size (S.fromList $ take n rest) = Just i
              | otherwise = go (i + 1) (tail rest)

day06a, day06b :: String :->: Int
day06a = MkSol {sParse = Just, sSolve = findSet 4, sPrint = show}
day06b = MkSol {sParse = Just, sSolve = findSet 14, sPrint = show}
