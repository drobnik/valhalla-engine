module Engine.Collision where

import Data.Int(Int32(..))

import qualified Debug.Trace as D

maxObj :: Int
maxObj = 10

maxLvl :: Int
maxLvl = 5

data BoxKind = TileGround | TileLava | TileSpikes | CollCoin | CollHealth
             | CollGate | CollPlayer deriving (Show, Eq, Ord)

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
collide (BoundingBox (top1, left1) (bottom1, right1))
  (BoundingBox (top2, left2) (bottom2, right2)) =
  not ((left2 > right1) || (right2 < left1) || (top2 > bottom1)
      || (bottom2 < top1))

type Bounds = ((Int32, Int32), (Int32, Int32)) --(x, y) (w, h)
data Quad = TopQuadR | BottomQuadR | TopQuadL | BottomQuadL | None
  deriving Show
-- to limit collision detection
data Quadtree = TEmpty Int Bounds
              | TLeaf Int Bounds [(BoundingBox, BoxKind)]
              | TNode Int Bounds Quadtree
                Quadtree Quadtree Quadtree
              deriving (Eq, Show)

newQuadtree :: Int -> Bounds -> Quadtree
newQuadtree lvl bounds' = TEmpty lvl bounds'

insert :: Int -> (BoundingBox, BoxKind) -> Quadtree -> Quadtree
insert lvl obj@(box', k') qtree = case qtree of
  TNode lvl pos@((x, y), (w, h)) n0 n1 n2 n3
    -> let
    verMid = x + (w `div` 2)
    horMid = y + (h `div` 2)
    (BoundingBox (xA, yA) (xB, yB)) = box'
    node0 -- if it fits, go deeper and add a value
      | xA < horMid && yB < verMid = insert (lvl + 1) obj n0
      | otherwise = n0 -- leave it alone
    node1
      | xA > horMid && yB < verMid = insert (lvl + 1) obj n1
      | otherwise = n1
    node2
      | xA < horMid && yB > verMid = insert (lvl + 1) obj n2
      | otherwise = n2
    node3
      | xA > x && yB > y = insert (lvl + 1) obj n3
      | otherwise = n3
    in TNode lvl pos node0 node1 node2 node3

  TLeaf lvl pos objs
    | lvl >= maxLvl
      -> TLeaf lvl pos (obj : objs)

  TEmpty lvl pos@((x, y), (w, h))
    | lvl >= maxLvl
      -> TLeaf lvl pos [obj]
    | otherwise -- there is still space for more - let's make a new node (split)
      -> let newW = w `div` 2
             newH = h `div` 2
             verMid = x + (w `div` 2)
             horMid = y + (h `div` 2)
             newNode = TNode lvl ((x, y), (w, h))
               (TEmpty (lvl + 1) ((x, y), (newW, newH)))
               (TEmpty (lvl + 1) (((x + horMid), y), (newW, newH)))
               (TEmpty (lvl + 1) ((x, (y + verMid)), (newW, newH)))
               (TEmpty (lvl + 1) (((x + horMid), (y + verMid)), (newW, newH)))
         in insert lvl obj newNode

insertElements :: [(BoundingBox, BoxKind)] -> Quadtree -> Quadtree
insertElements (x:xs) tree = insertElements xs (insert 0 x tree)
insertElements [] tree = tree

-- pass a bounding box of player and get other boxes in this quad
retrieve :: (BoundingBox, BoxKind) -> Quadtree -> [(BoundingBox, BoxKind)]
retrieve obj@(box', k) tree = case tree of
  TNode lvl pos@((x, y), (w, h)) n0 n1 n2 n3
    -> let
    verMid = x + (w `div` 2)
    horMid = y + (h `div` 2)
    (BoundingBox (xA, yA) (xB, yB)) = box'
    node
      | xA < horMid && yB < verMid = n0
      | xA > horMid && yB < verMid = n1
      | xA < horMid && yB > verMid = n2
      | xA > horMid && yB > verMid = n3
      | otherwise = TEmpty lvl pos
    in retrieve obj node
  TLeaf _ _ objs -> objs
  TEmpty _ _ -> []

checkCollisions :: BoundingBox -> [(BoundingBox, BoxKind)]
                -> [(BoundingBox, BoxKind)]
checkCollisions pBox = filter (\(box, _) -> collide pBox box )
