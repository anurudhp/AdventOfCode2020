import AOC.Helpers (countPred, unique)
import Control.Arrow ((>>>))
import Data.Functor (($>))
import Data.List (group, sort)
import qualified Data.Set as S
import Text.Parsec ((<|>), char, many, parse)

main :: IO ()
main =
  interact $ mkInput >>> (numBlackTilesAfter <$> [0, 100] <*>) . pure >>> show

getBlackTiles :: Input -> [Loc]
getBlackTiles = map head . filter (odd . length) . group . sort . map pathToLoc

numBlackTilesAfter :: Int -> Input -> Int
numBlackTilesAfter n = length . (!! n) . iterate updateGrid . getBlackTiles

updateGrid :: [Loc] -> [Loc]
updateGrid black =
  filter (predBlack . countBlackNeighbours) black ++
  filter (predWhite . countBlackNeighbours) white
  where
    blackS = S.fromList black
    white =
      unique . filter (`S.notMember` blackS) . concatMap neighbours $ black
    countBlackNeighbours = countPred (`S.member` blackS) . neighbours
    predBlack n = n == 1 || n == 2
    predWhite n = n == 2

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
dirToLoc _ = undefined

pathToLoc :: Path -> Loc
pathToLoc = foldl (<+>) (0, 0) . map dirToLoc

neighbours :: Loc -> [Loc]
neighbours l =
  map
    ((l <+>) . dirToLoc)
    [East, West, South East, South West, North East, North West]

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
