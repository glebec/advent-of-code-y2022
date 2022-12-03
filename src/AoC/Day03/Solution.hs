module AoC.Day03.Solution where

import AoC.Solver ((:->:)(..))

import Data.List.Split (chunksOf)
import Data.Set qualified as S

halve :: String -> (String, String)
halve s = splitAt (length s `div` 2) s

common2 :: (String, String) -> [Char]
common2 (l, r) = S.toList $ S.intersection (S.fromList l) (S.fromList r)

priority :: Char -> Int
priority c | fromEnum c >= 97 = fromEnum c - 96  -- [a..z]
           | fromEnum c >= 65 = fromEnum c - 38  -- [A..Z]
           | otherwise = 0

sackValue :: String -> Int
sackValue = sum . fmap priority . common2 . halve

day03a :: [String] :->: Int
day03a = MkSol
    { sParse = Just . lines
    , sSolve = Just . sum . fmap sackValue
    , sPrint = show
    }

letters :: S.Set Char
letters = S.fromList $ ['A'..'Z'] ++ ['a'..'z']

common3 :: [String] -> [Char]
common3 sacks = S.toList $ foldr S.intersection letters (fmap S.fromList sacks)

day03b :: [[String]] :->: Int
day03b = MkSol
    { sParse = Just . chunksOf 3 . lines
    , sSolve = Just . sum . fmap (sum . fmap priority . common3)
    , sPrint = show
    }
