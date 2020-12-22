{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Monad (zipWithM)
import Data.Functor
import Data.Maybe (catMaybes)
import Text.Parsec

main :: IO ()
main =
  interact $
  parseInput >>> ([getEarliest, getEarliestWithOffset] <*>) . pure >>> show

data Input =
  Input
    { getStart :: Int
    , getBuses :: [Maybe Int]
    }
  deriving (Show, Eq)

parseInput :: String -> Input
parseInput = either undefined id . parse readInput ""
  where
    readInput = Input <$> readStart <*> readBuses
    readStart = read <$> (many digit <* char '\n')
    readBuses = readBus `sepBy` char ','
    readBus = (char 'x' $> Nothing) <|> (Just . read <$> many digit)

-- part 1
getEarliest :: Input -> Int
getEarliest (Input st buses) =
  uncurry (*) $ foldl1 cmp [(wait i, i) | i <- catMaybes buses]
  where
    wait i = (i - (st `mod` i)) `mod` i
    cmp a b =
      if fst a < fst b
        then a
        else b

-- part 2
getEarliestWithOffset :: Input -> Int
getEarliestWithOffset =
  either undefined id .
  uncurry chineseRemainder .
  unzip .
  map (\(i, b) -> (rev i b, b)) .
  catMaybes . zipWith (\i -> ((i, ) <$>)) [0 ..] . getBuses
  where
    rev i b = (b - (i `mod` b)) `mod` b

-- https://rosettacode.org/wiki/Chinese_remainder_theorem#Haskell
egcd :: Int -> Int -> (Int, Int)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Int -> Int -> Either String Int
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b

chineseRemainder :: [Int] -> [Int] -> Either String Int
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii >>=
  (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii
