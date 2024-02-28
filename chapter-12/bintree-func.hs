-- Define an instance of the Functor class for the following type of binary trees that have data in
-- their nodes:

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show
-- Datatype definition for binary trees. 

instance Functor Tree where
    fmap _ Leaf = Leaf
    fmap g (Node m k o) = Node (fmap g m) (g k) (fmap g o)

