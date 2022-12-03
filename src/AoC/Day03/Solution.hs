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

day03a :: [Sack] :->: Int
day03a = MkSol
    { sParse = Just . lines
    , sSolve = Just . sum . fmap (commonValue . halve)
    , sPrint = show
    }

day03b :: [[Sack]] :->: Int
day03b = MkSol
    { sParse = Just . chunksOf 3 . lines
    , sSolve = Just . sum . fmap commonValue
    , sPrint = show
    }
