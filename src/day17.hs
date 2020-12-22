{-# LANGUAGE ParallelListComp #-}

import Control.Arrow ((>>>))
import Data.Bool (bool)
import Data.List (transpose)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Helpers (count)

main :: IO ()
main =
  interact $
  lines >>> (mkGrid <$> [3, 4] <*>) . pure >>> map (aliveAfter 6) >>> show

aliveAfter :: Int -> Grid -> Int
aliveAfter n = count Alive . stepN n . expandBy n

-- helpers
data Cell
  = Alive
  | Dead
  deriving (Eq, Show)

type Loc = [Int]

type Grid = M.Map Loc Cell

mkGrid :: Int -> [String] -> Grid
mkGrid d g =
  M.fromList $
  concat
    [ [ ([i, j] ++ replicate (d - 2) 0, bool Dead Alive (c == '#'))
      | c <- row
      | j <- [1 ..]
      ]
    | row <- g
    | i <- [1 ..]
    ]

type StepCellFn = Grid -> Loc -> Cell -> Cell

neighbours :: Loc -> Grid -> [Cell]
neighbours loc g = catMaybes $ M.lookup <$> nei <*> pure g
  where
    nei = filter (/= loc) $ mapM (\c -> [c - 1, c, c + 1]) loc

stepCell :: StepCellFn
stepCell g l cur
  | cur == Alive && occ `elem` [2, 3] = Alive
  | cur == Dead && occ == 3 = Alive
  | otherwise = Dead
  where
    occ = count Alive (neighbours l g)

expandBy :: Int -> Grid -> Grid
expandBy n g = foldl ins g keys
  where
    ins g' loc = M.insertWith (const id) loc Dead g'
    keys = mapM expandRange . transpose . M.keys $ g
    expandRange ps = [minimum ps - n .. maximum ps + n]

stepOnce :: Grid -> Grid
stepOnce g = M.mapWithKey (stepCell g) g

stepN :: Int -> Grid -> Grid
stepN n = (!! n) . iterate stepOnce
