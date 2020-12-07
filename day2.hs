import Control.Arrow ((>>>))
import Text.Parsec

main :: IO ()
main =
  interact $
  lines >>> (solve <$> [valid1, valid2] <*>) . pure >>> map show >>> unlines

solve :: Validator -> [String] -> Int
solve valid = length . filter id . map process
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

type Validator = Int -> Int -> Char -> String -> Bool

valid1 :: Validator
valid1 l r c s =
  let f = length [c' | c' <- s, c' == c]
   in l <= f && f <= r

valid2 :: Validator
valid2 l r c s
  | lc = not rc
  | otherwise = rc
  where
    lc = s !! (l - 1) == c
    rc = s !! (r - 1) == c
