-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTreeSolns ( Keymap,
                         size, depth,
                         get, set, del,
                         select,
                         toList, fromList,
                       )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth Leaf = 0
depth (Node _ _ left right) = 1 + (depth left `max` depth right )

-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList Leaf = []
toList (Node k v left right) = toList left ++ [(k,v)] ++ toList right

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f 
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key = f
    where
      f Leaf = Nothing
      f (Node k v left right) | key == k  = Just v
                              | key <= k  = f left
                              | otherwise = f right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList ((k,v):xs) = set k v (fromList xs)


-- Alternative higer-order solution:
--
-- fromList = foldr (uncurry set) Leaf

prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys


-- Optional Material -----------------------------------

-- Exercise 12

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT _ Leaf = Leaf
filterGT x (Node k v l r) | k == x = r
                             | k < x  = filterGT x r
                             | k > x  = Node k v (filterGT x l) r

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT _ Leaf = Leaf
filterLT x (Node k v l r) | k == x = l
                          | k < x  = Node k v l (filterLT x r)
                          | k > x  = filterLT x l
                                     
-- Exercise 13                                     
                                     
merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge Leaf t = t
merge t Leaf = t
merge x1@(Node k1 v1 l1 r1) x2@(Node k2 v2 l2 r2) 
  | k1 == k2  = Node k1 v1 (merge l1 l2) (merge r1 r2)
  | otherwise = Node k1 v1 (merge l1 (filterLT k1 x2)) (merge r1 (filterGT k1 x2))
                
prop_merge :: Keymap Int Int -> Keymap Int Int -> Bool                
prop_merge t1 t2 = sort (nubBy p (toList t1 ++ toList t2)) == toList (merge t1 t2)
  where p (k,_) (k',_) = k == k'

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del key = f
    where
      f Leaf = Leaf
      f (Node k v left right) | key == k  = merge left right
                              | key <= k  = Node k v (f left) right
                              | otherwise = Node k v left (f right)

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select _ Leaf = Leaf
select f (Node k v left right) 
    | f v       = Node k v (select f left) (select f right)
    | otherwise = merge (select f left) (select f right) 

-- Instances for QuickCheck -----------------------------

instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
