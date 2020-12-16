import Control.Arrow ((>>>))
import Data.Bool (bool)

main :: IO ()
main = interact $ const (part1 2020 input) >>> show

-- part 1
part1 :: Int -> [Int] -> Int
part1 n xs = getMove n (playGame n xs)

input :: [Int]
input = reverse [1, 12, 0, 20, 8, 16]

playMove :: [Int] -> [Int]
playMove (x:xs) =
  let ys = takeWhile (/= x) xs
   in bool 0 (1 + length ys) (length ys /= length xs) : x : xs

playGame :: Int -> [Int] -> [Int]
playGame n xs
  | length xs >= n = xs
  | otherwise = playGame n (playMove xs)

getMove :: Int -> [Int] -> Int
getMove n xs = xs !! (length xs - n)
