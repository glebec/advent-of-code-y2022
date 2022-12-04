module AoC.Day04.Solution where

import AoC.Solver ((:->:)(..))

import Numeric.Interval.NonEmpty (Interval, (...), contains)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String
type Sections = Interval Int

rangePair :: Parser (Sections, Sections)
rangePair = do
    r1 <- (...) <$> (decimal <* char '-') <*> (decimal <* char ',')
    r2 <- (...) <$> (decimal <* char '-') <*> (decimal <* space)
    pure (r1, r2)

eitherContains :: Ord a => (Interval a, Interval a) -> Bool
eitherContains (a, b) = a `contains` b || b `contains` a

day04a :: [(Sections, Sections)] :->: Int
day04a = MkSol
    { sParse = parseMaybe $ many rangePair
    , sSolve = Just . length . filter eitherContains
    , sPrint = show
    }
