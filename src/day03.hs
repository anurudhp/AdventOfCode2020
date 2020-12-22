import AOC.Helpers (chunksOf, count)
import Control.Arrow ((>>>))

main :: IO ()
main = interact $ lines >>> ([solve1, solve2] <*>) . pure >>> show

numTrees :: Int -> Int -> [String] -> Int
numTrees r d xs = count '#' (zipWith (!!) rows cols)
  where
    cols = [r * i `mod` length (head xs) | i <- [0 ..]]
    rows = head <$> chunksOf d xs

prodNumTrees :: [(Int, Int)] -> [String] -> Int
prodNumTrees mvs xs = product $ uncurry numTrees <$> mvs <*> pure xs

solve1, solve2 :: [String] -> Int
solve1 = numTrees 3 1

solve2 = prodNumTrees [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
