import Control.Arrow ((>>>))
import Data.List ((\\))
import Helpers (unique)
import Text.Parsec

main :: IO ()
main = interact $ readInput >>> ([sumAllInvalidValues] <*>) . pure >>> show

-- part 1
allRanges :: Input -> [Int]
allRanges = unique . concatMap snd . getRanges

invalidValues :: Range -> Ticket -> [Int]
invalidValues rs ts = [x | x <- ts, x `notElem` rs]

sumAllInvalidValues :: Input -> Int
sumAllInvalidValues ip =
  let r = allRanges ip
   in sum . map (sum . invalidValues r) . getTickets $ ip

type Ticket = [Int]

type Field = String

type Range = [Int]

data Input =
  Input
    { getRanges :: [(Field, Range)]
    , myTicket :: Ticket
    , getTickets :: [Ticket]
    }
  deriving (Show)

readInput :: String -> Input
readInput = either undefined id . parse parseInput ""
  where
    parseInput =
      Input <$> many (parseField <* char '\n') <*>
      (string "\nyour ticket:\n" *> parseTicket) <*>
      (string "\nnearby tickets:\n" *> many parseTicket)
    parseField =
      (,) <$> (many fieldChar <* string ": ") <*>
      ((++) <$> parseRange <*> (string " or " *> parseRange))
    fieldChar = choice [char c | c <- ' ' : ['a' .. 'z']]
    parseRange =
      mkRange <$> (read <$> many digit) <*> (read <$> (char '-' *> many digit))
    parseTicket = ((read <$> many digit) `sepBy1` char ',') <* char '\n'

mkRange :: Int -> Int -> [Int]
mkRange l u = [l .. u]
