module Engine.Collision where

import Data.Int(Int32(..))

class Ord a => Collidable a where
  boundingBox :: a -> BoundingBox

data BoundingBox = BoundingBox
                 { topLeftA :: (Int32, Int32)
                 , bottomRightB :: (Int32, Int32)
                 } deriving (Eq, Ord, Show)

makeBox :: Int -> Int -> Int32 -> Int32 -> BoundingBox --temp 1 arg
makeBox x y w h = BoundingBox (x', y') ((x' + w), (y' + h))
  where x' = fromIntegral x
        y' = fromIntegral y

collide :: BoundingBox -> BoundingBox -> Bool
collide (BoundingBox (lt1, top1) (rt1, bot1)) (BoundingBox (lt2, top2) (rt2, bot2)) =
  (lt1 > rt2) && (lt2 > rt1) && (bot1 > top2) && (bot2 > top1)

maxObj :: Int
maxObj = 10

maxLvl :: Int
maxLvl = 5

type Bounds = ((Int32, Int32), (Int32, Int32)) --(x, y) (w, h)
data Quad = TopQuadR | BottomQuadR | TopQuadL | BottomQuadL | None

-- to limit collision detection
data (Collidable a) => Quadtree a = Null | Quadtree Int [a] Bounds (Quadtree a)
                                           (Quadtree a) (Quadtree a) (Quadtree a)
                                           deriving Eq

newQuadtree :: (Collidable a) => Int -> Bounds -> Quadtree a
newQuadtree lvl bounds' = Quadtree lvl [] bounds' Null Null Null Null

split :: (Collidable a) => Quadtree a -> Quadtree a
split tree@(Quadtree lvl objs ((x, y), (boundW, boundH)) _ _ _ _) =
  let subWidth = boundW `div` 2
      subHeight = boundH `div` 2
      node0 = newQuadtree (lvl + 1) ((x + subWidth, y), (subWidth, subHeight))
      node1 = newQuadtree (lvl + 1) ((x, y + subHeight), (subWidth, subHeight))
      node2 = newQuadtree (lvl + 1) ((x, y + subHeight)
                                     , (subWidth, subHeight))
      node3 = newQuadtree (lvl + 1) ((x + subWidth, y + subHeight)
                                     , (subWidth, subHeight))
  in Quadtree lvl objs ((x, y), (boundW, boundH)) node0 node1 node2 node3

-- determinate the which node the object belongs to
getQuad :: (Collidable a) => a -> Bounds -> Quad
getQuad obj ((bX, bY), (bW, bH))
    | lefts = if topQuad then TopQuadL
              else if bottomQuad then BottomQuadL
                   else None
    | x > verMidpoint = if topQuad then TopQuadR
                        else if bottomQuad then BottomQuadR
                             else None
    | otherwise = None
    where
      (BoundingBox (x, y) (w, h)) = boundingBox obj
      verMidpoint = bX + (bW `div` 2)
      horMidpoint = bY + (bH `div` 2)
      topQuad = (y < horMidpoint) && ((y + h) < horMidpoint)
      bottomQuad = y > horMidpoint
      lefts = (x < verMidpoint) && (x + w < verMidpoint)

{-
-- mapping function
insert :: (Collidable a) => a -> Quadtree a -> Quadtree a
insert obj t@(Quadtree lvl objList bounds n0 n1 n2 n3) =
  if n0 /= Null
  then
    case getQuad obj bounds of
      None -> insertTree obj t
      TopQuadR -> insert obj n0
      TopQuadL -> insert obj n1
      BottomQuadL -> insert obj n2
      BottomQuadR -> insert obj n3
  else (Quadtree lvl objList bounds n0 n1 n2 n3)

insertTree :: (Collidable a) => a -> Quadtree a -> Quadtree a
insertTree obj t@(Quadtree lvl objList bounds n0 n1 n2 n3)
  | (length (obj : objList) > maxObj) && lvl < maxLvl =
      if n0 == Null then transTree obj (split t) else transTree obj t (obj:objList)
  | otherwise = Quadtree lvl (obj : objList) bounds n0 n1 n2 n3

transTree :: (Collidable a) => a -> Quadtree a -> [a] -> Quadtree a
transTree obj t@(Quadtree lvl obList bounds n0 n1 n2 n3) (x:xs) = undefined
transTree _ t [] = t
-}
