import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
  words >>>
  map readNavInst >>> ([finalLocXYSum, actualFinalLocXYSum] <*>) . pure >>> show

-- part 1
finalLocXYSum :: [NavInst] -> Int
finalLocXYSum ms =
  let loc = applyMoves ms (ShipLoc 0 0 East)
   in abs (getX loc) + abs (getY loc)

-- part 2
actualFinalLocXYSum :: [NavInst] -> Int
actualFinalLocXYSum ms =
  let loc = applyMoves' ms (ShipLoc' 0 0 10 1)
   in abs (getX' loc) + abs (getY' loc)

data NavDir
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

data NavInst
  = MoveFixed NavDir Int
  | Forw Int
  | TLeft Int
  | TRight Int
  deriving (Show, Eq)

readNavInst :: String -> NavInst
readNavInst s = dir (head s) (read (tail s))
  where
    dir 'N' = MoveFixed North
    dir 'S' = MoveFixed South
    dir 'E' = MoveFixed East
    dir 'W' = MoveFixed West
    dir 'L' = TLeft
    dir 'R' = TRight
    dir 'F' = Forw
    dir _ = undefined

data ShipLoc =
  ShipLoc
    { getX :: Int
    , getY :: Int
    , getDir :: NavDir
    }

rotLeft90 :: NavDir -> NavDir
rotLeft90 North = West
rotLeft90 West = South
rotLeft90 South = East
rotLeft90 East = North

moveShip :: NavInst -> ShipLoc -> ShipLoc
moveShip (MoveFixed North d) (ShipLoc x y f) = ShipLoc x (y + d) f
moveShip (MoveFixed South d) (ShipLoc x y f) = ShipLoc x (y - d) f
moveShip (MoveFixed East d) (ShipLoc x y f) = ShipLoc (x + d) y f
moveShip (MoveFixed West d) (ShipLoc x y f) = ShipLoc (x - d) y f
moveShip (Forw d) loc = moveShip (MoveFixed (getDir loc) d) loc
moveShip (TRight a) loc = moveShip (TLeft $ 360 - a) loc
moveShip (TLeft a) (ShipLoc x y f)
  | a == 90 = ShipLoc x y (rotLeft90 f)
  | a > 90 = moveShip (TLeft $ a - 90) $ ShipLoc x y (rotLeft90 f)
  | otherwise = undefined

applyMoves :: [NavInst] -> ShipLoc -> ShipLoc
applyMoves = foldr1 (.) . map moveShip . reverse

data ShipLoc' =
  ShipLoc'
    { getX' :: Int
    , getY' :: Int
    , getWayX :: Int
    , getWayY :: Int
    }

moveShip' :: NavInst -> ShipLoc' -> ShipLoc'
moveShip' (MoveFixed North d) (ShipLoc' x y wx wy) = ShipLoc' x y wx (wy + d)
moveShip' (MoveFixed South d) (ShipLoc' x y wx wy) = ShipLoc' x y wx (wy - d)
moveShip' (MoveFixed East d) (ShipLoc' x y wx wy) = ShipLoc' x y (wx + d) wy
moveShip' (MoveFixed West d) (ShipLoc' x y wx wy) = ShipLoc' x y (wx - d) wy
moveShip' (Forw d) (ShipLoc' x y wx wy) =
  ShipLoc' (x + d * wx) (y + d * wy) wx wy
moveShip' (TRight a) loc = moveShip' (TLeft $ 360 - a) loc
moveShip' (TLeft a) loc = rotShip a loc
  where
    rotBy90 (ShipLoc' x y wx wy) = ShipLoc' x y (-wy) wx
    rotShip a' loc'
      | a' == 0 = loc'
      | a' > 0 = rotShip (a' - 90) (rotBy90 loc')
      | otherwise = undefined

applyMoves' :: [NavInst] -> ShipLoc' -> ShipLoc'
applyMoves' = foldr1 (.) . map moveShip' . reverse
