-- The below document details on functors type class. 
-- The simple idea behind functors is that these are types which can apply a function to each element in the
-- type. They help generalizing functions for different types and help us write minimal programs using a very 
-- small set of explicit type variables. 


class Functor1 f where
    fmap1 :: (a -> b) -> f a -> f b

-- fmap1 is a function of normal type a -> b which takes an input of the type f a and can produce the generalized functor type result. 

instance Functor1 [] where
    fmap1 = map
    -- :t (fmap1) = (a -> b) -> [a] -> [b]

-- Addition function which performs addition on every element in the list.
add1 :: [Int] -> [Int]
add1 xs = fmap1 (+1) xs

data Maybe1 a = Just1 a | Nothing1 deriving (Show, Read) 

instance Functor1 Maybe1 where
    fmap1 :: (a -> b) -> Maybe1 a -> Maybe1 b
    fmap1 _ Nothing1 = Nothing1 
    fmap1 g (Just1 x) = Just1 (g x)


