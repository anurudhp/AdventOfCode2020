import Control.Arrow ((>>>))
import Data.List (group, intercalate, sort)
import Helpers (splitOn)

main :: IO ()
main =
  interact $
  lines >>>
  splitOn null >>> (solve <$> [answeredAny, answeredAll] <*>) . pure >>> show

solve :: ([String] -> Int) -> [[String]] -> Int
solve answered = sum . map answered

answeredAny :: [String] -> Int
answeredAny = length . group . sort . intercalate ""

answeredAll :: [String] -> Int
answeredAll = length . foldl1 intersect
  where
    intersect xs ys = [x | x <- xs, x `elem` ys]
