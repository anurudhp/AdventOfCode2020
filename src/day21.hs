{-# LANGUAGE TupleSections #-}

import AOC.Helpers (unique)
import Control.Category ((>>>))
import Data.List (intersect, sortOn)
import Data.List.Extra (groupOn)
import Text.Parsec (char, choice, many, parse, sepBy1, space, string)

main :: IO ()
main = interact $ mkInput >>> ([solve1] <*>) . pure >>> show

-- part 1
solve1 :: Input -> Int
solve1 = length . nonAllergenIngredients

nonAllergenIngredients :: Input -> [Ingredient]
nonAllergenIngredients ip =
  [i | i <- allIngredients ip, i `notElem` allergenIngredients ip]

allergenIngredients :: Input -> [Ingredient]
allergenIngredients =
  unique .
  concatMap (foldl1 intersect . map snd) .
  groupOn fst . sortOn fst . concatMap (\(i, a) -> map (, i) a)

-- helpers
allIngredients :: Input -> [Ingredient]
allIngredients = concatMap fst

-- allAllergens :: Input -> [Allergen]
-- allAllergens = concatMap snd
-- Input
type Ingredient = String

type Allergen = String

type Dish = ([Ingredient], [Allergen])

type Input = [Dish]

mkDish :: String -> Dish
mkDish = either undefined id . parse parseDish ""
  where
    parseDish = (,) <$> parseIngredients <*> parseAllergens
    parseIngredients = parseWord `sepBy1` space
    parseAllergens =
      string "(contains " *> (parseWord `sepBy1` string ", ") <* char ')'
    parseWord = many $ choice [char c | c <- ['a' .. 'z']]

mkInput :: String -> Input
mkInput = map mkDish . lines
