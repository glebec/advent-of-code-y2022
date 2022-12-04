module AoC.Day04.Solution where

import AoC.Solver ((:->:)(..))

import Data.IntegerInterval (IntegerInterval, (<=..<=), (==?), isSubsetOf)
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, parseMaybe)
import Text.Megaparsec.Char (char, space)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String
type Sections = IntegerInterval

sectionPair :: Parser (Sections, Sections)
sectionPair = do
    s1 <- (<=..<=) <$> (decimal <* char '-') <*> (decimal <* char ',')
    s2 <- (<=..<=) <$> (decimal <* char '-') <*> (decimal <* space)
    pure (s1, s2)

day04 :: ((Sections, Sections) -> Bool)  -- predicate on pairs
      -> ([(Sections, Sections)] :->: Int)  -- solution
day04 predicate = MkSol
    { sParse = parseMaybe $ many sectionPair
    , sSolve = Just . length . filter predicate
    , sPrint = show
    }

day04a, day04b :: [(Sections, Sections)] :->: Int
day04a = day04 $ \(a, b) -> a `isSubsetOf` b || b `isSubsetOf` a
day04b = day04 $ uncurry (==?)
