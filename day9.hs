import Control.Arrow ((>>>))
import Data.List (inits)
import Data.Maybe (mapMaybe)
import Data.Tuple.Extra (fst3)

main :: IO ()
main =
  interact $
  words >>>
  map read >>> ([getFirstInvalid, getPossibleSubsegment] <*>) . pure >>> show

-- part 1
getFirstInvalid :: [Int] -> Int
getFirstInvalid xs =
  head [x | (x, p) <- zip xs (inits xs), length p >= 25, not $ checkSum x p]
  where
    checkSum s ys = not $ null [True | a <- ys, b <- ys, a /= b, a + b == s]

-- part 2
getPossibleSubsegment :: [Int] -> Int
getPossibleSubsegment xs = head . mapMaybe compute $ inits xs
  where
    s = getFirstInvalid xs
    inf = 10 ^ 18
    f v (tot, lo, hi) = (tot + v, min lo v, max hi v)
    compute = extract . filter ((== s) . fst3) . scanr f (0, inf, 0)
      where
        extract [] = Nothing
        extract ((_, lo, hi):_) = Just $ lo + hi
