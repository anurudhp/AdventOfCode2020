import Control.Arrow ((>>>))
import Data.List ((\\), inits, sortOn, transpose)
import Text.Parsec

main :: IO ()
main =
  interact $ readInput >>> ([sumAllInvalidValues, part2] <*>) . pure >>> show

-- part 1
allRanges :: [Entry] -> Range
allRanges = concatMap snd

invalidValues :: Range -> Ticket -> [Int]
invalidValues rs ts = [x | x <- ts, not $ x `inRange` rs]

sumAllInvalidValues :: Input -> Int
sumAllInvalidValues (Input entries _ tickets) =
  let range = allRanges entries
   in sum . concatMap (invalidValues range) $ tickets

-- part 2 :: product of all departure fields
isTicketValid :: Range -> Ticket -> Bool
isTicketValid rs = all (`inRange` rs)

part2 :: Input -> Int
part2 (Input entries ticket tickets) =
  product [v | (v, fs) <- poss', isDeparture (head fs)]
  where
    range = allRanges entries
    validTickets = filter (isTicketValid range) tickets
    poss =
      sortOn (length . snd) . zip ticket $
      [ [name | (name, rs) <- entries, all (`inRange` rs) col]
      | col <- transpose validTickets
      ]
    poss' = zipWith removeUsed poss (map (concatMap snd) $ inits poss)
    removeUsed (v, fs) pfs = (v, fs \\ pfs)
    isDeparture = and . zipWith (==) "departure"

-- helpers
type Ticket = [Int]

type Field = String

type Range = [(Int, Int)]

inRange :: Int -> Range -> Bool
inRange x = any (\(l, r) -> l <= x && x <= r)

type Entry = (Field, Range)

data Input =
  Input
    { getEntries :: [Entry]
    , myTicket :: Ticket
    , getTickets :: [Ticket]
    }
  deriving (Show)

readInput :: String -> Input
readInput = either undefined id . parse parseInput ""
  where
    parseInput =
      Input <$> many (parseField <* newline) <*>
      (string "\nyour ticket:\n" *> parseTicket) <*>
      (string "\nnearby tickets:\n" *> many parseTicket)
    parseField =
      (,) <$> (many fieldChar <* string ": ") <*>
      (parseRange `sepBy1` string " or ")
    fieldChar = choice [char c | c <- ' ' : ['a' .. 'z']]
    parseRange = (,) <$> parseInt <*> (char '-' *> parseInt)
    parseTicket = (parseInt `sepBy1` char ',') <* newline
    parseInt = read <$> many digit
