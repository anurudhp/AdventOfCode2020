import Control.Arrow ((>>>))
import Data.Bits
import qualified Data.Map.Strict as M
import Helpers
import Text.Parsec

main :: IO ()
main =
  interact $
  readProgram >>> (finalSum <$> [applyOp, applyOp'] <*>) . pure >>> show

type ApplyOp = Inst -> Env -> Env

runProgram :: ApplyOp -> [Inst] -> Env
runProgram ap = ($ emptyEnv) . foldl1 (.) . reverse . map ap

finalSum :: ApplyOp -> [Inst] -> Int
finalSum ap = sum . getMem . runProgram ap

data Inst
  = Mask String -- and or
  | MemUpd Int Int -- i v

type Mem = M.Map Int Int

data Env =
  Env
    { getMask :: String
    , getMem :: Mem
    }

readProgram :: String -> [Inst]
readProgram = either (error . show) id . parse parseProg ""
  where
    parseProg = many parseInst
    parseInst = char 'm' *> (parseMask <|> parseMemUpd) <* char '\n'
    parseMask = Mask <$> (string "ask = " *> many maskChar)
    parseMemUpd =
      MemUpd <$> (string "em[" *> (read <$> many digit) <* string "] = ") <*>
      (read <$> many digit)
    maskChar = char 'X' <|> char '0' <|> char '1'

emptyEnv :: Env
emptyEnv = Env undefined M.empty

applyOp :: Inst -> Env -> Env
applyOp (Mask mask) (Env _ mem) = Env mask mem
applyOp (MemUpd i v) (Env mask mem) =
  Env mask $ M.insert i ((v .|. o) .&. a) mem
  where
    a = strToBin $ translate 'X' '1' mask
    o = strToBin $ translate 'X' '0' mask

applyOp' :: Inst -> Env -> Env
applyOp' (Mask mask) (Env _ mem) = Env mask mem
applyOp' (MemUpd i v) (Env mask mem) = Env mask $ updMem idxs mem
  where
    updMem = foldl1 (.) . reverse . map upd
    idxs = map strToBin $ mapM convBit $ zipWith updBit (binToStr i) mask
    upd i' = M.insert i' v
    updBit b '0' = b
    updBit _ b = b
    convBit '0' = "0"
    convBit '1' = "1"
    convBit 'X' = "01"
