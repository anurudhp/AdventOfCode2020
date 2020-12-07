module Helpers where

countPred :: Eq a => (a -> Bool) -> [a] -> Int
countPred p = length . filter p

count :: Eq a => a -> [a] -> Int
count x = countPred (== x)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (hs, ts) = splitAt n xs
   in hs : chunksOf n ts

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn p (x:xs)
  | p x = [] : g : gs
  | otherwise = (x : g) : gs
  where
    (g:gs) = splitOn p xs
