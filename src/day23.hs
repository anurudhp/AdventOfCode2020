import Control.Arrow ((>>>))
import Data.List (sort)
import Data.List.HT (chop)

main :: IO ()
main =
  interact $
  const input >>> mkInput >>> ([solve1] <*>) . pure >>> map mkOutput >>> show

-- part 1
solve1 :: [Int] -> [Int]
solve1 = tail . rotTo 1 . playGame 100
  where
    rotTo x = (x :) . concat . reverse . chop (== x)

-- helpers
playMove :: [Int] -> [Int]
playMove (x:xs) = ls ++ [y] ++ picked ++ rs ++ [x]
  where
    (picked, xs') = splitAt 3 xs
    y = head . concatMap (reverse . sort) $ filter <$> [(< x), (> x)] <*> [xs']
    [ls, rs] = chop (== y) xs'

playGame :: Int -> [Int] -> [Int]
playGame n = (!! n) . iterate playMove

-- IO
input :: String
input = "523764819"

mkInput :: String -> [Int]
mkInput = map $ read . (: "")

mkOutput :: [Int] -> String
mkOutput = concatMap show
