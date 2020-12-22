{-# LANGUAGE FlexibleContexts #-}

import Control.Arrow ((>>>))
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Text.Parsec

main :: IO ()
main = interact $ mkInput >>> ([matchAll 0] <*>) . pure >>> show

-- part 1
matchAll :: Int -> Input -> Int
matchAll = undefined

-- helpers
data Rule
  = Const Char
  | Rule [[Int]]

type Grammar = M.Map Int Rule

type Input = (Grammar, [String])

mkInput :: String -> Input
mkInput = either undefined id . parse parseInput ""
  where
    parseInput = (,) <$> parseGrammar <*> (newline *> many parseQuery)
    parseGrammar = M.fromList <$> many (parseRule <* newline)
    parseRule =
      (,) <$> (read <$> (many digit <* string ": ")) <*>
      (Const <$> parseConst <|> (Rule <$> parseOptions `sepBy1` string " | "))
    parseConst = char '"' *> anyChar <* char '"'
    parseOptions = parseInt `sepBy1` space
    parseQuery = manyTill anyChar newline
    parseInt = read <$> many digit

runRule :: Int -> Grammar -> String -> Bool
runRule n g = either (const False) (const True) . parse (rule n) ""
  where
    rule i =
      case g ! i of
        (Const c) -> char c
        (Rule r) -> foldl1 (<|>) . map (foldl1 (*>) . map rule) $ r
