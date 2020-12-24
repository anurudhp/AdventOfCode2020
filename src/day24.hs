import AOC.Helpers (countPred)
import Control.Arrow ((>>>))
import Data.Functor (($>))
import Data.List (group, sort)
import Debug.Trace (trace)
import Text.Parsec ((<|>), char, many, parse)

main :: IO ()
main = interact $ mkInput >>> ([solve1] <*>) . pure >>> show

-- part 1
solve1 :: Input -> Int
solve1 = countPred (odd . length) . group . sort . map pathToLoc

-- helpers
data Dir
  = East
  | West
  | South Dir
  | North Dir
  deriving (Eq, Show)

type Path = [Dir]

type Loc = (Int, Int)

(<+>) :: Loc -> Loc -> Loc
(x, y) <+> (x', y') = (x + x', y + y')

dirToLoc :: Dir -> Loc
dirToLoc East = (1, 0)
dirToLoc West = (-1, 0)
dirToLoc (South East) = (0, -1)
dirToLoc (South West) = (-1, -1)
dirToLoc (North East) = (1, 1)
dirToLoc (North West) = (0, 1)

pathToLoc :: Path -> Loc
pathToLoc = foldl (<+>) (0, 0) . map dirToLoc

-- Input
type Input = [Path]

mkPath :: String -> Path
mkPath = either undefined id . parse parsePath ""
  where
    parsePath = many parseDir
    parseDir = parseEast <|> parseWest <|> parseSouth <|> parseNorth
    parseEast = char 'e' $> East
    parseWest = char 'w' $> West
    parseSouth = South <$> (char 's' *> parseDir)
    parseNorth = North <$> (char 'n' *> parseDir)

mkInput :: String -> Input
mkInput = lines >>> map mkPath
