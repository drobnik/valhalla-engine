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

-- | Enum used for indicating where
data Quad = TopQuadR
          | BottomQuadR
          | TopQuadL
          | BottomQuadL
          | None
  deriving Show

-- | Initialize an empty tree with given bounds
newQuadtree :: Int -> Bounds -> Quadtree
newQuadtree lvl bounds' = TEmpty lvl bounds'

-- | A naive function for collistion detection.
collide :: BoundingBox -- ^ A box to collide with
        -> BoundingBox -- ^ A moving box - 'Player'
        -> Bool        -- ^ Flag for triggering collision
collide (BoundingBox (top1, left1) (bottom1, right1))
  (BoundingBox (top2, left2) (bottom2, right2)) =
  (not ((left2 > right1)
         || (right2 < left1)
         || (top2 > bottom1)
         || (bottom2 < top1)))

insert :: Int -> (BoundingBox, BoxKind) -> Quadtree -> Quadtree
insert lvl obj@(box', k') qtree =
  case qtree of
    -- | If we enter the node, we want to see in which quad
    -- we are in. To do that, we define horizontalMid and verticalMid which
    -- stand for the positions in two dimensions.
    TNode lvl pos@((x, y), (w, h)) n0 n1 n2 n3 ->
      let
        verMid = x + (w `div` 2)
        horMid = y + (h `div` 2)
        (BoundingBox (xA, yA) (xB, yB)) = box'

        -- | Check for every node if passed box fits in the bounds.
        -- If this is the case, go deeper to add itsel
        -- If not, simply go deeper and find a proper tree to retrieve.
        node0
          | xA < horMid && yB < verMid = insert (lvl + 1) obj n0
          | otherwise = n0
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

    -- ^ If we are in the leaf, add 'BoundingBox' no matter what
    TLeaf lvl pos objs -> TLeaf lvl pos (obj : objs)

    -- | If we have entered an uninitialized tree, we check current depth level.
    -- If it exceeds the limit - spawn a leaf and return
    TEmpty lvl pos@((x, y), (w, h))
      | lvl >= maxLvl
        -> TLeaf lvl pos [obj]

    -- | In other case we know there is still some space for more trees.
    -- A new node is made with half size of original tree boundings.
      | otherwise
        -> let newW = w `div` 2
               newH = h `div` 2
               verMid = x + (w `div` 2)
               horMid = y + (h `div` 2)
               newNode = TNode lvl ((x, y), (w, h))
                         (TEmpty (lvl + 1) ((x, y), (newW, newH)))
                         (TEmpty (lvl + 1) (((x + horMid), y), (newW, newH)))
                         (TEmpty (lvl + 1) ((x, (y + verMid)), (newW, newH)))
                         (TEmpty (lvl + 1) (((x + horMid), (y + verMid))
                                           , (newW, newH)))
           in insert lvl obj newNode

-- | An insert function for a list of 'BoundingBox'es.
insertElements :: [(BoundingBox, BoxKind)] -> Quadtree -> Quadtree
insertElements (x:xs) tree = insertElements xs (insert 0 x tree)
insertElements [] tree = tree


-- | Pass a bounding box of player and get other boxes in this quad
retrieve :: (BoundingBox, BoxKind)   -- ^ 'BoundingBox' entering the quad'
         -> Quadtree                 -- ^ Quad tree storing all 'BoundingBox'es
         -> [(BoundingBox, BoxKind)] -- ^ 'BoundingBox'es most likely to collide with
retrieve obj@(box', k) tree =
  case tree of
    -- | Check in which quater box is located. In every step recursively
    -- divide the bounds and enter the quad which is most suitable.
    -- Go deeper till you reach an empty tree or a leaf.
    TNode lvl pos@((x, y), (w, h)) n0 n1 n2 n3 ->
      let
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

    -- ^ When a leaf is reached, function returns a list of 'BoundingBox'es
    TLeaf _ _ objs -> objs
    -- ^ On empty tree an empty list is returned
    TEmpty _ _ -> []

-- | Filter out the boxes which do not collide with 'Player'
checkCollisions :: BoundingBox              -- ^ 'BoundingBox' to collide with
                -> [(BoundingBox, BoxKind)] -- ^ List of potentially colliding boxes
                -> [(BoundingBox, BoxKind)] -- ^ List of colliding 'BoundingBox'es
checkCollisions pBox = filter (\(box, _) -> collide pBox box )
