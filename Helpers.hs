module Helpers where

-- count number of elements satisfying the predicate
countPred :: Eq a => (a -> Bool) -> [a] -> Int
countPred p = length . filter p

-- count number of elements equal to a value
count :: Eq a => a -> [a] -> Int
count x = countPred (== x)

-- split into chunks of fixed size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (hs, ts) = splitAt n xs
   in hs : chunksOf n ts

-- split on predicate
-- all values satisfying the predicate are dropped
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn p (x:xs)
  | p x = [] : g : gs
  | otherwise = (x : g) : gs
  where
    (g:gs) = splitOn p xs

-- Convert a binary string to a number
strToBin :: String -> Int
strToBin = foldl (\acc b -> 2 * acc + b) 0 . map bit
  where
    bit '0' = 0
    bit '1' = 1
