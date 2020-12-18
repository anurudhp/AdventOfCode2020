module Helpers where

import Data.Bool (bool)
import Data.List

-- count number of elements satisfying the predicate
countPred :: Foldable t => (a -> Bool) -> t a -> Int
countPred p = foldl (flip (bool id (1 +) . p)) 0

-- count number of elements equal to a value
count :: (Foldable t, Eq a) => a -> t a -> Int
count x = countPred (== x)

-- unique elements from a list
unique :: Ord a => [a] -> [a]
unique = map head . group . sort

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
    bit _ = undefined

-- Convert integer to binary string
-- tail recursive version
binToStrTailRec :: Int -> String -> String
binToStrTailRec n suf
  | n == 0 = '0' : suf
  | n == 1 = '1' : suf
  | even n = binToStrTailRec (n `div` 2) ('0' : suf)
  | odd n = binToStrTailRec (n `div` 2) ('1' : suf)
  | otherwise = undefined

-- Convert integer to binary string
binToStr :: Int -> String
binToStr n = binToStrTailRec n ""

-- apply a single value translation
-- replace each occurrence of x with x'
translate :: (Functor f, Eq a) => a -> a -> f a -> f a
translate x x' = fmap conv
  where
    conv y
      | y == x = x'
      | otherwise = y
