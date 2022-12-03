module AoC.Day03.Solution where

import AoC.Solver ((:->:)(..))

import Data.Set qualified as S

halve :: String -> (String, String)
halve s = splitAt (length s `div` 2) s

common :: (String, String) -> [Char]
common (l, r) = S.toList $ S.intersection (S.fromList l) (S.fromList r)

priority :: Char -> Int
priority c | fromEnum c >= 97 = fromEnum c - 96  -- [a..z]
        | fromEnum c >= 65 = fromEnum c - 38  -- [A..Z]
        | otherwise = 0

sackValue :: String -> Int
sackValue = sum . fmap priority . common . halve

day03a :: [String] :->: Int
day03a = MkSol
    { sParse = Just . lines
    , sSolve = Just . sum . fmap sackValue
    , sPrint = show
    }
