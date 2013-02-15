{-# LANGUAGE NoImplicitPrelude #-}
module Tree where

import Primitive 

import Prelude (Show, Read, error, show)

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Read)

emptyTree :: Tree a 
emptyTree = Leaf

addToRoot :: a -> Tree a -> Tree a 
addToRoot l ts  = Node l ts Leaf 

addToLeft :: a -> Tree a -> Tree a 
addToLeft li Leaf = Node li Leaf Leaf
addToLeft li (Node x ls rs) = Node x (addToLeft li ls) rs  

addToRight :: a -> Tree a -> Tree a
addToRight li Leaf = Node li Leaf Leaf 
addToRight li (Node x ls rs) = Node x ls (addToRight li rs)

mergeTreeToRight :: Tree a -> Tree a -> Tree a 
mergeTreeToRight Leaf ys = ys 
mergeTreeToRight xs Leaf = xs 
mergeTreeToRight (Node x ls rs) ys = Node x ls (mergeTreeToRight rs ys)

leftRotate :: Tree a -> Tree a 
leftRotate (Node x ls (Node l lt rt)) = Node l (Node x ls lt) rt
leftRotate t = error "leftRotate: Incorrect tree"

rightRotate :: Tree a -> Tree a 
rightRotate (Node x (Node r lt rt) rs) = Node x lt (Node r rt rs)
rightRotate t = error "rightRotate: Incorrect tree"

tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Leaf = Leaf 
tmap f (Node x ls rs) = Node (f x) (tmap f ls) (tmap f rs)

tconcatMap :: (a -> Tree b) -> Tree a -> Tree b
tconcatMap _ Leaf = Leaf
tconcatMap f (Node x ls rs) = mergeTreeToRight (tconcatMap f ls) $ mergeTreeToRight (f x) (tconcatMap f rs)    

tfold :: (a -> b -> b) -> b -> Tree a -> b
tfold _ acc Leaf = acc
tfold f acc (Node x ls rs) = f x $ tfold f (tfold f acc rs) ls  

-- Всё что угодно, главное, чтобы соответствовало
-- заданию
