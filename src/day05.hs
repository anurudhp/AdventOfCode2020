import AOC.Helpers (strToBin)
import Control.Arrow ((>>>))
import Data.List ((\\), sort)

main :: IO ()
main = interact $ lines >>> ([highestSeat, findMySeat] <*>) . pure >>> show

-- part 1
highestSeat :: [String] -> Int
highestSeat = maximum . map seatToID

-- part 2
findMySeat :: [String] -> Int
findMySeat seats = head $ range \\ occ
  where
    occ = sort . map seatToID $ seats
    range = [minimum occ .. maximum occ]

seatToID :: String -> Int
seatToID = strToBin . map conv
  where
    conv 'F' = '0'
    conv 'B' = '1'
    conv 'L' = '0'
    conv 'R' = '1'
