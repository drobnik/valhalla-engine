-- | 'Engine.Collision' exports an interface to work on collision system and
-- provides definitions for 'QuadTree' and 'BoundingBox'.
-- _elaborate on AABB here_. The provided algorithm for collision detection is naive
-- and is not inteded to be used in real platorm games.

module Engine.Collision
  (
    -- * Collision-specific types
    BoundingBox(BoundingBox)
  , BoxKind(..)
  , Collidable(..)
    -- * Collision handling
  , makeBox
  , checkCollisions
    -- * Quad Tree Management
  , newQuadtree
  , insertElements
  , retrieve
  ) where

import Data.Int(Int32(..))
import Engine.Consts(maxObj
                    , maxLvl)

-- | Type class for all game entities having 'BoundingBox'.
-- Thus able to collide with one another.
class Ord a => Collidable a where
  boundingBox :: a -> BoundingBox

-- | Temporary data to indicate the kind of 'BoundingBox' with which the collision
-- has occured. It is a game-specific information and should be used in collision
-- response not here.
data BoxKind = TileGround
             | TileLava
             | TileSpikes
             | CollCoin
             | CollHealth
             | CollGate
             | CollPlayer
             deriving (Show, Eq, Ord)

-- | Data to indicate where the oversection of AABB boxes occured.
data CorrPos = CLeft
             | CRight
             | CUp
             | CDown

-- | For a tile-based game which uses no rotative objects,
-- a method of Axis-Aligned Bounding Box (AABB) can be used.
-- In this approach every entity is approximated to a box,
-- described by two points: A(Top-Left, the lowest x and y)
-- and B(Bottom-Right, A value with added with and height of
-- 'BoundingBox')
data BoundingBox = BoundingBox
                 { topLeftA     :: (Int32, Int32)
                 , bottomRightB :: (Int32, Int32)
                 } deriving (Eq, Ord, Show)

-- | Construct a 'BoundingBox' with particular dimensions,
-- at given point A
makeBox :: Int        -- ^ x of the point A
        -> Int        -- ^ y of the point A
        -> Int32      -- ^ Width
        -> Int32      -- ^ Height
        -> BoundingBox
makeBox x y w h = BoundingBox (x', y') ((x' + w), (y' + h))
  where x' = fromIntegral x
        y' = fromIntegral y

---------------------------------------------------------------------
--
-- A Quad Tree data structure is used to divide a 2D region recursively
-- by subdividing the plane into four parts called quads.
-- Subnodes are named accordingly as the quads in Cartesian
-- coordinates. The collision is checked only in a particular quad
-- where the moving object (i.e. player) is. The maximum depth can be set
-- manually or based on the maximum number of objects in the quad.
--
---------------------------------------------------------------------

-- | A Quad tree definition.
data Quadtree =
  -- | A new tree. Stores size of 2D quad and 0 level depth.
  TEmpty Int Bounds

  -- | The deepest quad. Stores current level depth, size of 2D quad
  -- and the list of collidable onjects. They are described by
  -- 'BoundingBox' and its 'BoxKind'.
  | TLeaf Int Bounds [(BoundingBox, BoxKind)]
  -- | An inner node with four children and its 'Bounds'
  | TNode Int Bounds Quadtree Quadtree Quadtree Quadtree
  deriving (Eq, Show)

-- | The bounds of the quad given by the lowest (x, y) coordinates
-- and dimensions.
type Bounds = ((Int32, Int32), (Int32, Int32)) --(x, y) (w, h)

data Quad = TopQuadR | BottomQuadR | TopQuadL | BottomQuadL | None
  deriving Show


newQuadtree :: Int -> Bounds -> Quadtree
newQuadtree lvl bounds' = TEmpty lvl bounds'


-- | A naive function for collistion detection
collide :: BoundingBox -- ^ A box to collide with
        -> BoundingBox -- ^ A moving box ('Player')
        -> Bool        -- ^ Flag for triggering collision
collide (BoundingBox (top1, left1) (bottom1, right1))
  (BoundingBox (top2, left2) (bottom2, right2)) =
  (not ((left2 > right1)
         || (right2 < left1)
         || (top2 > bottom1)
         || (bottom2 < top1)))

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
