import AOC.Helpers (countPred)
import Control.Arrow ((>>>))
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as M
import Data.Map.Strict ((!))
import Debug.Trace (trace)
import Text.Parsec

main :: IO ()
main = interact $ mkInput >>> ([matchAll 0] <*>) . pure >>> show

-- part 1
matchAll :: Int -> Input -> Int
matchAll n (grammar, ips) = countPred (runRule n grammar) (take 5 ips)

-- helpers
data Rule
  = Const Char
  | Rule [[Int]]

type Grammar = M.Map Int Rule

type Input = (Grammar, [String])

mkInput :: String -> Input
mkInput = either (flip trace undefined . show) id . parse parseInput ""
  where
    parseInput = (,) <$> parseGrammar <*> (newline *> many parseQuery)
    parseGrammar = M.fromList <$> many (parseRule <* newline)
    parseRule =
      (,) <$> (parseInt <* string ": ") <*>
      ((Const <$> parseConst) <|>
       (Rule <$> parseOptions `sepBy1` try (string "| ")))
    parseConst = char '"' *> anyChar <* char '"'
    parseOptions = parseInt `sepBy1` char ' '
    parseQuery = manyTill anyChar newline
    parseInt = read <$> many digit

runRule :: Int -> Grammar -> String -> Bool
runRule n g s =
  trace ("RUN " ++ s) . either (const False) (const True) . parse (rule n) "" $
  s
  where
    rule :: Int -> ParsecT String u Identity Bool
    rule i =
      case g ! i of
        (Const c) -> char c $> trace (show i) True
        (Rule r) -> foldl1 (<|>) . map (foldl1 (*>) . map rule) $ r
