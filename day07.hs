import Control.Arrow ((>>>))
import Data.Bifunctor (Bifunctor(first))
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Helpers (countPred)

main :: IO ()
main =
  interact $
  lines >>>
  processRules >>>
  ([countAncestors "shiny-gold", totalInside . findBagByName "shiny-gold"] <*>) .
  pure >>>
  map show >>> unlines

data Bag =
  Bag String [(Bag, Int)]

type BagInfo = (String, [(String, Int)])

-- part1
countAncestors :: String -> [Bag] -> Int
countAncestors b = subtract 1 . countPred ok
  where
    ok (Bag name conn)
      | name == b = True
      | otherwise = any (ok . fst) conn

-- part2
findBagByName :: String -> [Bag] -> Bag
findBagByName name = head . filter match
  where
    match (Bag name' _) = name == name'

totalInside :: Bag -> Int
totalInside (Bag _ sub) = sum [n * (1 + totalInside b) | (b, n) <- sub]

-- process
processRules :: [String] -> [Bag]
processRules xs = snd . foldr (mkBag . fst) (M.empty, []) $ info
  where
    info = map processRule xs
    infoMap = M.fromList info
    mkBag name (mp, bags) =
      let mp' = addBag name mp
       in (mp', mp' ! name : bags)
    addBag name mp
      | M.member name mp = mp
      | otherwise =
        let conn = infoMap ! name
         in let mp' = foldr (addBag . fst) mp conn
             in let bag = Bag name $ map (first (mp' !)) conn
                 in M.insert name bag mp'

processRule :: String -> BagInfo
processRule rule = (name, inside $ drop 4 ws)
  where
    ws = words rule
    name = (ws !! 0) ++ "-" ++ (ws !! 1)
    inside [] = []
    inside ("no":_) = []
    inside (n:attr:col:bs) = (attr ++ "-" ++ col, read n) : inside (tail bs)
