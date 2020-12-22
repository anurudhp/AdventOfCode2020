import Control.Arrow ((>>>))
import Data.List (sort)
import Helpers (count)

main :: IO ()
main =
  interact $ words >>> map read >>> sort >>> ([part1, ways] <*>) . pure >>> show

-- part 1
part1 :: [Int] -> Integer
part1 xs = toInteger $ count 1 diffs * (1 + count 3 diffs)
  where
    diffs = zipWith (-) xs (0 : xs)

-- part 2
ways :: [Int] -> Integer
ways xs = head . foldl compute [1] $ xs
  where
    compute dp x = dpx : dp
      where
        dpx = sum [dpx' | (dpx', x') <- zip (reverse dp) (0 : xs), x - x' <= 3]
