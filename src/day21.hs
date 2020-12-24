{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

import AOC.Helpers (unique)
import Control.Category ((>>>))
import Data.List (intercalate, intersect, sortOn)
import Data.List.Extra (groupOn)
import Text.Parsec (char, choice, many, parse, sepBy1, space, string)

main :: IO ()
main = interact $ mkInput >>> ([solve1, solve2] <*>) . pure >>> unlines

-- part 1
solve1 :: Input -> String
solve1 = show . length . nonAllergenIngredients

nonAllergenIngredients :: Input -> [Ingredient]
nonAllergenIngredients ip =
  [i | i <- allIngredients ip, i `notElem` allAllergenIngredients ip]

allAllergenIngredients :: Input -> [Ingredient]
allAllergenIngredients = unique . concatMap snd . allergenIngredients

allergenIngredients :: Input -> [(Allergen, [Ingredient])]
allergenIngredients =
  map (\xs -> (fst (head xs), foldl1 intersect . map snd $ xs)) .
  groupOn fst . sortOn fst . concatMap (\(i, a) -> map (, i) a)

-- part 2
dangerousIngredientList :: Input -> [Ingredient]
dangerousIngredientList =
  head . filter isValid . mapM snd . sortOn fst . allergenIngredients
  where
    isValid xs = length (unique xs) == length xs

solve2 :: Input -> String
solve2 = intercalate "," . dangerousIngredientList

-- helpers
allIngredients :: Input -> [Ingredient]
allIngredients = concatMap fst

-- Input
type Ingredient = String

type Allergen = String

type Dish = ([Ingredient], [Allergen])

type Input = [Dish]

mkDish :: String -> Dish
mkDish = either undefined id . parse parseDish ""
  where
    parseDish = (,) <$> fil parseIngredients <*> fil parseAllergens
    parseIngredients = parseWord `sepBy1` space
    parseAllergens =
      string "(contains " *> (parseWord `sepBy1` string ", ") <* char ')'
    parseWord = many $ choice [char c | c <- ['a' .. 'z']]
    fil = fmap (filter (not . null))

mkInput :: String -> Input
mkInput = map mkDish . lines
