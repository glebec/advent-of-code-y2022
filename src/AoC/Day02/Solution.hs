module AoC.Day02.Solution where

import AoC.Solver ((:->:)(..))

import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), many, parseMaybe)
import Text.Megaparsec.Char (char, space)

data Sign = Rock | Paper | Scissors deriving (Eq, Enum, Show)
data Outcome = Loss | Tie | Win deriving (Enum, Show)

type Parser = Parsec Void String

pSign :: Parser Sign
pSign =
    (char 'A' <|> char 'X') $> Rock <|>
    (char 'B' <|> char 'Y') $> Paper <|>
    (char 'C' <|> char 'Z') $> Scissors

pOutcome :: Parser Outcome
pOutcome =
    char 'X' $> Loss <|>
    char 'Y' $> Tie <|>
    char 'Z' $> Win

gamesA :: Parser [(Sign, Sign)]
gamesA = many $ do
    elfSign <- pSign <* space
    mySign <- pSign <* space
    pure (elfSign, mySign)

gamesB :: Parser [(Sign, Outcome)]
gamesB = many $ do
    elfSign <- pSign <* space
    outcome <- pOutcome <* space
    pure (elfSign, outcome)

winnerAgainst :: Sign -> Sign
winnerAgainst Rock = Paper
winnerAgainst Paper = Scissors
winnerAgainst Scissors = Rock

losesTo :: Sign -> Sign
losesTo Rock = Scissors
losesTo Scissors = Paper
losesTo Paper = Rock

choose :: Outcome -> Sign -> Sign
choose Win s = winnerAgainst s
choose Loss s = losesTo s
choose Tie s = s

play :: (Sign, Sign) -> Outcome
play game@(elf, me) = case game of
    (Rock,     Paper)    -> Win
    (Paper,    Scissors) -> Win
    (Scissors, Rock)     -> Win
    _ | elf == me        -> Tie
    _                    -> Loss

signValue :: Sign -> Int
signValue = (+1) . fromEnum

outcomeValue :: Outcome -> Int
outcomeValue = \case
  Loss -> 0
  Tie -> 3
  Win -> 6

roundScoreA :: (Sign, Sign) -> Int
roundScoreA game@(_, me) = signValue me + outcomeValue (play game)

roundScoreB :: (Sign, Outcome) -> Int
roundScoreB (elfSign, outcome) =
    signValue (choose outcome elfSign) + outcomeValue outcome

day02a :: [(Sign, Sign)] :->: Int
day02a = MkSol
    { sParse = parseMaybe gamesA
    , sSolve = Just . sum . fmap roundScoreA
    , sPrint = show
    }

day02b :: [(Sign, Outcome)] :->: Int
day02b = MkSol
    { sParse = parseMaybe gamesB
    , sSolve = Just . sum . fmap roundScoreB
    , sPrint = show
    }
