import Control.Arrow ((>>>))
import Helpers (splitOn)

main :: IO ()
main = interact $ mkInput >>> ([part1] <*>) . pure >>> show

-- part 1
part1 :: GameState -> Int
part1 =
  playGame >>>
  filter (not . null) >>> head >>> reverse >>> zipWith (*) [1 ..] >>> sum

-- helpers
type GameState = [[Int]]

mkInput :: String -> GameState
mkInput = lines >>> splitOn null >>> map (tail >>> map read)

playMove :: GameState -> GameState
playMove [x:xs, y:ys]
  | x < y = [xs, ys ++ [y, x]]
  | otherwise = [xs ++ [x, y], ys]

playGame :: GameState -> GameState
playGame ip
  | any null ip = ip
  | otherwise = playGame (playMove ip)
