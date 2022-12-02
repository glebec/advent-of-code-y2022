module AoC.Day02.Solution where

import AoC.Solver ((:->:)(..))

import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, (<|>), many, parseMaybe)
import Text.Megaparsec.Char (char, string, space, space1)
import Text.Megaparsec.Char.Lexer (decimal)

data Sign = Rock | Paper | Scissors deriving (Eq, Enum, Show)
data Outcome = Loss | Tie | Win deriving (Enum, Show)

type Parser = Parsec Void String

sign :: Parser Sign
sign =
    char 'A' $> Rock <|>
    char 'B' $> Paper <|>
    char 'C' $> Scissors <|>
    char 'X' $> Rock <|>
    char 'Y' $> Paper <|>
    char 'Z' $> Scissors

games :: Parser [(Sign, Sign)]
games = many $ do
    elfSign <- sign
    space
    mySign <- sign
    space
    pure (elfSign, mySign)

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

roundScore :: (Sign, Sign) -> Int
roundScore game@(_, me) = signValue me + outcomeValue (play game)

day02a :: [(Sign, Sign)] :->: Int
day02a = MkSol
    { sParse = parseMaybe games
    , sSolve = Just . sum . fmap roundScore
    , sPrint = show
    }

-- day01b :: [(Sign, Sign)] :->: Int
-- day01b = MkSol
--     { sParse = parseMaybe games
--     , sSolve = Just . undefined
--     , sPrint = show
--     }
