module Engine.Collision where

import Data.Int(Int32(..))
import Data.Set (Set(..))

type Bounds = ((Int32, Int32), (Int32, Int32)) --(x, y) (w, h)

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

-- to limit collision detection
data (Collidable a) => Quadtree a = Quadtree
                                    { maxObjects :: Int --how many objects in a node
                                    , maxLevel :: Int --deepest lvl of subnode
                                    , level :: Int
                                    , objects :: Set a
                                    , bounds :: Bounds
                                    , node0 :: Quadtree a
                                    , node1 :: Quadtree a
                                    , node2 :: Quadtree a
                                    , node3 :: Quadtree a
                                    }
