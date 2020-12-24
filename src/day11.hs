{-# LANGUAGE ParallelListComp #-}

import AOC.Helpers (count)
import Control.Arrow ((>>>))
import Control.Monad.Loops (iterateUntilM)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)

main :: IO ()
main =
  interact $
  lines >>>
  mkGrid >>>
  (stepTillStable . stepCell <$> [neighbours1, neighbours2] <*>) . pure >>>
  map (count Occupied) >>> show

type StepCellFn = Grid -> Loc -> Cell -> Cell

type NeighbourCells = Loc -> Grid -> [Cell]

stepCell :: NeighbourCells -> StepCellFn
stepCell neighbours g l cur
  | cur == Empty && occ == 0 = Occupied
  | cur == Occupied && occ >= 4 = Empty
  | otherwise = cur
  where
    occ = count Occupied (neighbours l g)

-- part 1
neighbours1 :: Loc -> Grid -> [Cell]
neighbours1 (x, y) g =
  catMaybes $
  M.lookup <$>
  [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0] <*>
  pure g

-- part 2
neighbours2 :: Loc -> Grid -> [Cell]
neighbours2 (x, y) g =
  catMaybes
    [ head . (++ [Nothing]) . dropWhile (== Just Floor) $
    [M.lookup (x + dx * r, y + dy * r) g | r <- [1 .. 100]]
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dx /= 0 || dy /= 0
    ]

-- helpers
data Cell
  = Floor
  | Empty
  | Occupied
  deriving (Eq, Show)

type Loc = (Int, Int)

type Grid = M.Map Loc Cell

mkCell :: Char -> Cell
mkCell '.' = Floor
mkCell 'L' = Empty
mkCell '#' = Occupied
mkCell _ = undefined

mkGrid :: [String] -> Grid
mkGrid ip =
  M.fromList $
  concat
    [[((i, j), mkCell c) | j <- [1 ..] | c <- row] | i <- [1 ..] | row <- ip]

stepGrid :: StepCellFn -> Grid -> Grid
stepGrid step g = M.mapWithKey (step g) g

stepTillStable :: StepCellFn -> Grid -> Grid
stepTillStable step =
  either id undefined . iterateUntilM (const False) (stepNew step)

stepNew :: StepCellFn -> Grid -> Either Grid Grid
stepNew step g =
  let g' = stepGrid step g
   in if g == g'
        then Left g
        else Right g'
