module AoC.Day03.Solution where

import AoC.Solver ((:->:)(..))

import Data.List.Split (chunksOf)
import Data.Set qualified as S

type Sack = String
type Item = Char

items :: S.Set Item
items = S.fromList $ ['A'..'Z'] ++ ['a'..'z']

common :: [Sack] -> [Item]
common sacks = S.toList $ foldr S.intersection items (fmap S.fromList sacks)

halve :: Sack -> [Sack]
halve sack = [l, r] where (l, r) = splitAt (length sack `div` 2) sack

priority :: Item -> Int
priority item
    | n >= 97   = n - 96  -- [a..z]
    | n >= 65   = n - 38  -- [A..Z]
    | otherwise = 0
    where n = fromEnum item

commonValue :: [Sack] -> Int
commonValue = sum . fmap priority . common

day03 :: ([Sack] -> [[Sack]])  -- how to group sacks
      -> ([[Sack]] :->: Int)  -- solution
day03 groupSacks = MkSol
    { sParse = Just . groupSacks . lines
    , sSolve = Just . sum . fmap commonValue
    , sPrint = show
    }

day03a, day03b :: [[Sack]] :->: Int
day03a = day03 $ fmap halve
day03b = day03 $ chunksOf 3
