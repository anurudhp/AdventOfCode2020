import Control.Arrow ((>>>))
import Data.List
import Data.Maybe
import Text.Read

main :: IO ()
main =
  interact $
  lines >>>
  splitOn null >>> map unwords >>> map checkValid >>> count True >>> show

checkValid = checkValidFull

-- part 1
checkValidBasic :: String -> Bool
checkValidBasic =
  null .
  (["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] \\) .
  map (head . splitOn (== ':')) . words

-- part 2
checkValidFull :: String -> Bool
checkValidFull = isJust . parsePassport

data EyeColor
  = Amb
  | Blu
  | Brn
  | Gry
  | Grn
  | Hzl
  | Oth
  deriving (Show)

parseEyeColor :: String -> Maybe EyeColor
parseEyeColor "amb" = Just Amb
parseEyeColor "blu" = Just Blu
parseEyeColor "brn" = Just Brn
parseEyeColor "gry" = Just Gry
parseEyeColor "grn" = Just Grn
parseEyeColor "hzl" = Just Hzl
parseEyeColor "oth" = Just Oth
parseEyeColor _ = Nothing

parseHexColor :: String -> Maybe String
parseHexColor s
  | length s /= 7 = Nothing
  | head s /= '#' = Nothing
  | any (`notElem` ['0' .. '9'] ++ ['a' .. 'f']) (tail s) = Nothing
  | otherwise = Just $ tail s

parsePID :: String -> Maybe String
parsePID s
  | length s == 9 && all (`elem` ['0' .. '9']) s = Just s
  | otherwise = Nothing

parseByr :: Int -> Maybe Int
parseByr y
  | 1920 <= y && y <= 2002 = Just y
  | otherwise = Nothing

parseIyr :: Int -> Maybe Int
parseIyr y
  | 2010 <= y && y <= 2020 = Just y
  | otherwise = Nothing

parseEyr :: Int -> Maybe Int
parseEyr y
  | 2020 <= y && y <= 2030 = Just y
  | otherwise = Nothing

type Height = (Int, String)

parseHgt :: String -> Maybe Height
parseHgt s
  | length s < 2 = Nothing
  | u == "cm" =
    if 150 <= h && h <= 193
      then Just (h, u)
      else Nothing
  | u == "in" =
    if 59 <= h && h <= 76
      then Just (h, u)
      else Nothing
  | otherwise = Nothing
  where
    (hs, u) = splitAt (length s - 2) s
    h = read hs

data Passport =
  Passport
    { byr :: Int
    , iyr :: Int
    , eyr :: Int
    , hgt :: Height
    , hcl :: String
    , ecl :: EyeColor
    , pid :: String
    }
  deriving (Show)

parsePassport :: String -> Maybe Passport
parsePassport s =
  Passport <$> (get "byr" >>= readMaybe >>= parseByr) <*>
  (get "iyr" >>= readMaybe >>= parseIyr) <*>
  (get "eyr" >>= readMaybe >>= parseEyr) <*>
  (get "hgt" >>= parseHgt) <*>
  (get "hcl" >>= parseHexColor) <*>
  (get "ecl" >>= parseEyeColor) <*>
  (get "pid" >>= parsePID)
  where
    kvs = map ((\[k, v] -> (k, v)) . splitOn (== ':')) . words $ s
    get k = lookup k kvs

-- Helpers
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn pred (x:xs)
  | pred x = [] : g : gs
  | otherwise = (x : g) : gs
  where
    (g:gs) = splitOn pred xs
