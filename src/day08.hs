{-# LANGUAGE ParallelListComp #-}

import Control.Arrow ((>>>))
import Data.List (inits, tails)
import Data.Maybe (mapMaybe)

main :: IO ()
main =
  interact $
  readProg >>> ([valueOnRepeat, valueOnTerminate] <*>) . pure >>> show

-- part 1
stepTillDone :: ExecState -> ExecState
stepTillDone st =
  let en = step st
   in if status en == InProgress
        then stepTillDone en
        else en

valueOnRepeat :: [Inst] -> Int
valueOnRepeat = acc . stepTillDone . mkProg

-- part 2
getIfTerminates :: ExecState -> Maybe Int
getIfTerminates st =
  let en = stepTillDone st
   in if status en == Done
        then Just (acc en)
        else Nothing

flipSingleNopJmp :: [Inst] -> [[Inst]]
flipSingleNopJmp is =
  [ pre ++ flipNopJmpInst cur : suf
  | pre <- inits is
  | cur <- is
  | suf <- tail $ tails is
  ]
  where
    flipNopJmp Nop = Jmp
    flipNopJmp Jmp = Nop
    flipNopJmp op = op
    flipNopJmpInst (Inst op arg count) = Inst (flipNopJmp op) arg count

valueOnTerminate :: [Inst] -> Int
valueOnTerminate = head . mapMaybe (getIfTerminates . mkProg) . flipSingleNopJmp

-- grammar and semantics
data Op
  = Nop
  | Acc
  | Jmp
  deriving (Show, Eq)

readOp :: String -> Op
readOp "nop" = Nop
readOp "acc" = Acc
readOp "jmp" = Jmp
readOp _ = undefined

data Inst =
  Inst
    { getOp :: Op
    , getArg :: Int
    , usedCount :: Int
    }
  deriving (Show, Eq)

readInst :: String -> Inst
readInst inst =
  let [op, arg] = words inst
   in Inst (readOp op) (readI arg) 0
  where
    readI ('+':n) = read n
    readI ('-':n) = -(read n)
    readI _ = undefined

updateCount :: Inst -> Inst
updateCount (Inst op arg count) = Inst op arg (count + 1)

readProg :: String -> [Inst]
readProg = map readInst . lines

mkProg :: [Inst] -> ExecState
mkProg inst = ExecState [] (head inst) (tail inst) 0 InProgress

data Status
  = Done
  | Loop
  | InProgress
  deriving (Show, Eq)

data ExecState =
  ExecState
    { tapeLeft :: [Inst]
    , curInst :: Inst
    , tapeRight :: [Inst]
    , acc :: Int
    , status :: Status
    }
  deriving (Show, Eq)

accValue :: Int -> ExecState -> ExecState
accValue inc (ExecState l c r a s) = ExecState l c r (a + inc) s

moveRight :: ExecState -> ExecState
moveRight es@(ExecState l c r a s)
  | s /= InProgress = es
  | null r = ExecState l c r a Done
  | otherwise = ExecState (c : l) (head r) (tail r) a s

moveLeft :: ExecState -> ExecState
moveLeft es@(ExecState l c r a s)
  | s /= InProgress = es
  | null l = ExecState l c r a Done
  | otherwise = ExecState (tail l) (head l) (c : r) a s

moveN :: Int -> ExecState -> ExecState
moveN n s
  | n > 0 = iterate moveRight s !! n
  | n < 0 = iterate moveLeft s !! (-n)
  | otherwise = s

step :: ExecState -> ExecState
step es@(ExecState l c r a s)
  | s /= InProgress = es
  | usedCount c /= 0 = ExecState l c r a Loop
  | otherwise =
    let c' = updateCount c
     in let es' = ExecState l c' r a s
         in case getOp c of
              Nop -> moveRight es'
              Acc -> moveRight $ accValue (getArg c) es'
              Jmp -> moveN (getArg c) es'
