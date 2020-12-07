import Control.Arrow ((>>>))
import Data.List (group, intercalate, sort)
import Helpers (splitOn)

main :: IO ()
main = interact $ lines >>> splitOn null >>> part1 >>> show

-- part1
part1 :: [[String]] -> Int
part1 = sum . map answered

answered :: [String] -> Int
answered = length . group . sort . intercalate ""
