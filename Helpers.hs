module Helpers where

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (hs, ts) = splitAt n xs
   in hs : chunksOf n ts

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn pred (x:xs)
  | pred x = [] : g : gs
  | otherwise = (x : g) : gs
  where
    (g:gs) = splitOn pred xs
