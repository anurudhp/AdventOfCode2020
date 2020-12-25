import Control.Category ((>>>))

main :: IO ()
main = interact $ const input >>> ([findEncKey] <*>) . pure >>> show

-- part 1
findEncKey :: [Int] -> Int
findEncKey [ec, ed] = ec ^% (ed `dlog` 7)

-- helpers
genSubGroup :: Int -> [Int]
genSubGroup g = iterate (*% g) 1

dlog :: Int -> Int -> Int
dlog n g = elemAtIndex n (genSubGroup g)

elemAtIndex :: (Eq a) => a -> [a] -> Int
elemAtIndex x = fst . head . dropWhile ((/= x) . snd) . zip [0 ..]

-- Input
input :: [Int]
input = [19774466, 7290641]

-- modular arithmetic
type MInt = Int

modv :: Int
modv = 20201227

(+%) :: MInt -> MInt -> MInt
a +% b = (a + b) `mod` modv

infixl 6 +%

(*%) :: MInt -> MInt -> MInt
a *% b = (a * b) `mod` modv

infixl 7 *%

(^%) :: MInt -> MInt -> MInt
a ^% n
  | n == 0 = 1
  | odd n = a *% a ^% (n - 1)
  | even n =
    let u = a ^% (n `div` 2)
     in u *% u

infixl 8 ^%
