module AoC.Day03.Solution where

import AoC.Solver ((:->:)(..))

import Data.List.Split (chunksOf)
import Data.Set qualified as S

letters :: S.Set Char
letters = S.fromList $ ['A'..'Z'] ++ ['a'..'z']

common :: [String] -> [Char]
common sacks = S.toList $ foldr S.intersection letters (fmap S.fromList sacks)

halve :: String -> [String]
halve s = let (l, r) = splitAt (length s `div` 2) s in [l, r]

priority :: Char -> Int
priority c | n >= 97 = n - 96  -- [a..z]
           | n >= 65 = n - 38  -- [A..Z]
           | otherwise = 0
           where n = fromEnum c

commonValue :: [String] -> Int
commonValue = sum . fmap priority . common

day03a :: [String] :->: Int
day03a = MkSol
    { sParse = Just . lines
    , sSolve = Just . sum . fmap (commonValue . halve)
    , sPrint = show
    }

day03b :: [[String]] :->: Int
day03b = MkSol
    { sParse = Just . chunksOf 3 . lines
    , sSolve = Just . sum . fmap commonValue
    , sPrint = show
    }
