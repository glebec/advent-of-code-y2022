module AoC.Day05.Solution where

import AoC.Solver ((:->:)(..))

import Data.IntMap.Strict qualified as IM
import Data.List (foldl', transpose)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Text.Megaparsec (
    Parsec,
    anySingle,
    manyTill,
    parseMaybe,
    sepBy1,
    skipCount,
    some,
    (<|>),
 )
import Text.Megaparsec.Char (char, letterChar, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String
type Crate = Char
type Stack = [Crate]
type Stacks = IM.IntMap Stack

data Move = Move
    { howMany :: Int
    , source :: Int
    , dest :: Int
    }

-- Parsing

crate :: Parser Crate
crate = char '[' *> letterChar <* char ']'

crateSlot :: Parser (Maybe Crate)
crateSlot = (Just <$> crate) <|> (Nothing <$ string "   ")

crateRow :: Parser [Maybe Crate]
crateRow = (crateSlot `sepBy1` char ' ') <* newline

crateStacks :: Parser Stacks
crateStacks = do
    rows <- some crateRow
    let columns = transpose rows
    let stacks = catMaybes <$> columns
    pure $ IM.fromAscList (zip [1..] stacks)

move :: Parser Move
move = do
    n <- string "move " *> decimal
    f <- string " from " *> decimal
    t <- string " to " *> decimal
    pure $ Move n f t

stacksAndMoves :: Parser (Stacks, [Move])
stacksAndMoves = do
    s <- crateStacks
    skipCount 2 $ manyTill anySingle newline
    m <- some $ move <* space
    pure (s, m)

-- Processing

applyMove
    :: ([Crate] -> [Crate])  -- optional crane modifier
    -> Stacks -> Move -> Stacks
applyMove modifier initial (Move {howMany, source, dest}) = final where
    final = IM.adjust (modifier taken ++) dest intermediate
    intermediate = IM.adjust (const remainder) source initial
    (taken, remainder) = splitAt howMany (initial IM.! source)

processA, processB :: (Stacks, [Move]) -> Stacks
processA (stacks, moves) = foldl' (applyMove reverse) stacks moves
processB (stacks, moves) = foldl' (applyMove id) stacks moves

tops :: Stacks -> [Crate]
tops stacks = head . snd <$> IM.toAscList stacks

-- Solutions

day05a, day05b :: (Stacks, [Move]) :->: String
day05a = day05b { sSolve = Just . tops . processA }
day05b = MkSol
    { sParse = parseMaybe stacksAndMoves
    , sSolve = Just . tops . processB
    , sPrint = id
    }
