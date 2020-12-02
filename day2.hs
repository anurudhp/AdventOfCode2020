import Control.Arrow ((>>>))
import Text.Parsec

main :: IO ()
main = interact $ lines >>> solve >>> show

solve :: [String] -> Int
solve = length . filter id . map process
  where
    process :: String -> Bool
    process line =
      let (Right v) = parse parser "" line
       in v
    parser = do
      l <- read <$> many digit
      char '-'
      r <- read <$> many digit
      char ' '
      c <- anyChar
      string ": "
      s <- many anyChar
      return $ valid l r c s

-- select part
valid = valid1

valid1 :: Int -> Int -> Char -> String -> Bool
valid1 l r c s =
  let f = length [c' | c' <- s, c' == c]
   in l <= f && f <= r

valid2 :: Int -> Int -> Char -> String -> Bool
valid2 l r c s
  | lc = not rc
  | otherwise = rc
  where
    lc = s !! (l - 1) == c
    rc = s !! (r - 1) == c
