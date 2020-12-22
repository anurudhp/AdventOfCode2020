import Control.Arrow ((>>>))
import Data.Functor (($>))
import Text.Parsec

main :: IO ()
main =
  interact $
  lines >>>
  (sumAllResults <$> [evalExprNoPrec, evalExprPrec] <*>) . pure >>> show

sumAllResults :: (String -> Int) -> [String] -> Int
sumAllResults evalExpr = sum . map evalExpr

-- part 1
evalExprNoPrec :: String -> Int
evalExprNoPrec = either undefined id . parse parseExpr ""
  where
    parseExpr = foldl (flip ($)) <$> parseTerm <*> many parseROp
    parseROp = (char ' ' *> parseOp) <*> (char ' ' *> parseTerm)
    parseTerm = (char '(' *> parseExpr <* char ')') <|> parseNum
    parseNum = read <$> many digit
    parseOp = parseAdd <|> parseMul
    parseAdd = char '+' $> (+)
    parseMul = char '*' $> (*)

-- part 1
evalExprPrec :: String -> Int
evalExprPrec = either (error . show) id . parse parseExpr ""
  where
    parseExpr = parseProd
    parseProd = foldl (flip ($)) <$> parseSum <*> many parseRMul
    parseRMul = (*) <$> (string " * " *> parseSum)
    parseSum = foldl (flip ($)) <$> parseTerm <*> many (try parseRAdd)
    parseRAdd = (+) <$> (string " + " *> parseTerm)
    parseTerm = (char '(' *> parseExpr <* char ')') <|> parseNum
    parseNum = read <$> many digit
