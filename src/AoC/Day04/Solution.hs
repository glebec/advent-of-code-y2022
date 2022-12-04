module AoC.Day04.Solution where

import AoC.Solver ((:->:)(..))

import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String
type Sections = (Int, Int)

sectionPair :: Parser (Sections, Sections)
sectionPair = do
    s1 <- (,) <$> (decimal <* char '-') <*> (decimal <* char ',')
    s2 <- (,) <$> (decimal <* char '-') <*> (decimal <* space)
    pure (s1, s2)

eitherContains :: (Sections, Sections) -> Bool
eitherContains (a, b) = a `contains` b || b `contains` a
    where contains (xLo, xHi) (yLo, yHi) = xLo <= yLo && yHi <= xHi

overlaps :: (Sections, Sections) -> Bool
overlaps pair@(a, b) =
    eitherContains pair
    || a `lowOverlaps` b
    || b `lowOverlaps` a
    where lowOverlaps (xLo, xHi) (yLo, _) = xLo <= yLo && yLo <= xHi

day04 :: ((Sections, Sections) -> Bool)  -- predicate on pairs
      -> ([(Sections, Sections)] :->: Int)  -- solution
day04 predicate = MkSol
    { sParse = parseMaybe $ many sectionPair
    , sSolve = Just . length . filter predicate
    , sPrint = show
    }

day04a, day04b :: [(Sections, Sections)] :->: Int
day04a = day04 eitherContains
day04b = day04 overlaps
