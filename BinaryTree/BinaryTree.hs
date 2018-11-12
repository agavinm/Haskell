-- *****************************************************************
-- File:   BinaryTree.hs
-- Author: Andrés Gavín Murillo 716358
-- Author: Eduardo Gimeno Soriano 721615
-- Date:   June 2018
-- Coms:   TecProg - Prac. 6 - Haskell
--         Module BinaryTree
-- *****************************************************************

module BinaryTree where

import Data.List

data Tree t = NULL | Branch (Tree t) t (Tree t)

instance (Show t) => Show (Tree t) where
    show NULL = "NULL"
    show (Branch lb x rb) = "(" ++ show lb ++ " |" ++ show x ++ "| " ++ show rb ++ ")"
    

empty :: Tree t
empty = NULL

leaf :: (Ord t) => t -> Tree t
leaf x = Branch NULL x NULL

tree :: (Ord t) => t -> Tree t -> Tree t -> Tree t
tree x lb rb = Branch lb x rb

size :: (Ord t) => Tree t -> Integer
size NULL = 0
size (Branch lb x rb) = 1 + (size lb) + (size rb)


preorder :: (Ord t) => Tree t -> [t]
preorder NULL = []
preorder (Branch lb x rb) = [x] ++ preorder lb ++ preorder rb

postorder :: (Ord t) => Tree t -> [t]
postorder NULL = []
postorder (Branch lb x rb) = postorder lb ++ postorder rb ++ [x]

inorder :: (Ord t) => Tree t -> [t]
inorder NULL = []
inorder (Branch lb x rb) = inorder lb ++ [x] ++ inorder rb


add :: (Ord t) => Tree t -> t -> Tree t
add NULL x = leaf x
add (Branch lb t rb) x
 | t == x = tree t lb rb
 | t < x  = tree t lb (add rb x)
 | t > x  = tree t (add lb x) rb

between :: (Ord t) => Tree t -> t -> t -> [t]
between NULL xmin xmax = []
between (Branch lb x rb) xmin xmax
 | (x >= xmin) && (x <= xmax) = [x] ++ between lb xmin xmax ++ between rb xmin xmax
 | x < xmin = between rb xmin xmax
 | x > xmax = between lb xmin xmax
