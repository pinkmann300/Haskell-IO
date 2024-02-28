-- The below document details on functors and applicatives type class. 
-- The simple idea behind functors is that these are types which can apply a function to each element in the
-- type. They help generalizing functions for different types and help us write minimal programs using a very 
-- small set of explicit type variables. 

class Functor1 f where
    fmap1 :: (a -> b) -> f a -> f b
    -- fmap1 is a function of normal type a -> b which takes an input of the type f a and can produce the generalized functor type result. 

instance Functor1 [] where
    fmap1 :: (a -> b) -> [a] -> [b]
    fmap1 = map
    -- :t (fmap1) = (a -> b) -> [a] -> [b]

-- Addition function which performs addition on every element in the list.
add1 :: [Int] -> [Int]
add1 = fmap1 (+1)

data Maybe1 a = Just1 a | Nothing1 deriving (Show, Read) 

instance Functor1 Maybe1 where
    fmap1 :: (a -> b) -> Maybe1 a -> Maybe1 b
    fmap1 _ Nothing1 = Nothing1 
    fmap1 g (Just1 x) = Just1 (g x)

-- Generalizing functions using the Functor type class for instances of the Functor type class

inc1 :: Functor1 f => f Int -> f Int
inc1 = fmap1 (+1)

-- Functors are required to satisfy 2 laws : The identity law and the composition law

class Functor1 f => Applicative1 f where
    pure1 :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b 

instance Applicative1 Maybe1 where 
    pure1 = Just1 
    Nothing1 <**> _ = Nothing1
    (Just1 g) <**> mx = fmap1 g mx

instance Applicative1 [] where 
    pure1 x = [x]
    gs <**> xs = [g x | g <- gs, x <- xs]




    
