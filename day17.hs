{-# LANGUAGE ParallelListComp #-}

import Control.Arrow ((>>>))
import Data.Bool (bool)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Tuple.Extra (fst3, snd3, thd3)
import Helpers (count)

main :: IO ()
main = interact $ lines >>> mkGrid >>> ([aliveAfter 6] <*>) . pure >>> show

-- part 1
aliveAfter :: Int -> Grid -> Int
aliveAfter n = count Alive . stepN n . expandBy n

-- helpers
data Cell
  = Alive
  | Dead
  deriving (Eq, Show)

type Loc = (Int, Int, Int)

type Grid = M.Map Loc Cell

mkGrid :: [String] -> Grid
mkGrid g =
  M.fromList $
  concat
    [ [((i, j, 0), bool Dead Alive (c == '#')) | c <- row | j <- [1 ..]]
    | row <- g
    | i <- [1 ..]
    ]

type StepCellFn = Grid -> Loc -> Cell -> Cell

neighbours :: Loc -> Grid -> [Cell]
neighbours (x, y, z) g =
  catMaybes $
  M.lookup <$>
  [ (x + dx, y + dy, z + dz)
  | dx <- [-1, 0, 1]
  , dy <- [-1, 0, 1]
  , dz <- [-1, 0, 1]
  , dx /= 0 || dy /= 0 || dz /= 0
  ] <*>
  pure g

stepCell :: StepCellFn
stepCell g l cur
  | cur == Alive && occ `elem` [2, 3] = Alive
  | cur == Dead && occ == 3 = Alive
  | otherwise = Dead
  where
    occ = count Alive (neighbours l g)

expandBy :: Int -> Grid -> Grid
expandBy n g = foldl ins g locs
  where
    ins g' loc = M.insertWith (const id) loc Dead g'
    keys = M.keys g
    xs = map fst3 keys
    ys = map snd3 keys
    zs = map thd3 keys
    expandedRange ps = [minimum ps - n .. maximum ps + n]
    locs =
      [ (x, y, z)
      | x <- expandedRange xs
      , y <- expandedRange ys
      , z <- expandedRange zs
      ]

stepOnce :: Grid -> Grid
stepOnce g = M.mapWithKey (stepCell g) g

stepN :: Int -> Grid -> Grid
stepN n = (!! n) . iterate stepOnce
