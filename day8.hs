import Control.Arrow ((>>>))

main :: IO ()
main = interact $ readProg >>> mkProg >>> ([stepTillRepeat] <*>) . pure >>> show

stepTillRepeat :: ExecState -> Int
stepTillRepeat s =
  case stepNew s of
    Nothing -> acc s
    Just s' -> stepTillRepeat s'

data Op
  = Nop
  | Acc
  | Jmp
  deriving (Show)

readOp :: String -> Op
readOp "nop" = Nop
readOp "acc" = Acc
readOp "jmp" = Jmp

data Inst =
  Inst
    { getOp :: Op
    , getArg :: Int
    , usedCount :: Int
    }
  deriving (Show)

readInst :: String -> Inst
readInst inst =
  let [op, arg] = words inst
   in Inst (readOp op) (readI arg) 0
  where
    readI ('+':n) = read n
    readI ('-':n) = -(read n)

updateCount :: Inst -> Inst
updateCount (Inst op arg count) = Inst op arg (count + 1)

readProg :: String -> [Inst]
readProg = map readInst . lines

mkProg :: [Inst] -> ExecState
mkProg inst = ExecState [] (head inst) (tail inst) 0

data ExecState =
  ExecState
    { tapeLeft :: [Inst]
    , curInst :: Inst
    , tapeRight :: [Inst]
    , acc :: Int
    }
  deriving (Show)

accValue :: Int -> ExecState -> ExecState
accValue inc (ExecState l c r a) = ExecState l c r (a + inc)

moveRight :: ExecState -> ExecState
moveRight (ExecState l c r a) = ExecState (c : l) (head r) (tail r) a

moveLeft :: ExecState -> ExecState
moveLeft (ExecState l c r a) = ExecState (tail l) (head l) (c : r) a

moveN :: Int -> ExecState -> ExecState
moveN n s
  | n == 0 = s
  | n > 0 = iterate moveRight s !! n
  | n < 0 = iterate moveLeft s !! (-n)

step :: ExecState -> ExecState
step (ExecState l c r a) =
  let c' = updateCount c
   in let s = ExecState l c' r a
       in case getOp c of
            Nop -> moveRight s
            Acc -> moveRight $ accValue (getArg c) s
            Jmp -> moveN (getArg c) s

stepNew :: ExecState -> Maybe ExecState
stepNew s
  | usedCount (curInst s) == 0 = Just $ step s
  | otherwise = Nothing
