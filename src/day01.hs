import Control.Arrow ((>>>))

main :: IO ()
main = interact $ words >>> map read >>> ([solve1, solve2] <*>) . pure >>> show

solve1, solve2 :: [Int] -> Int
solve1 xs = head [x * y | x <- xs, y <- xs, x + y == 2020]

solve2 xs = head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]
