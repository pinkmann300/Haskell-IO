-- Complete the following instance declaration to make the partially-applied function type (a ->)
-- into a functor:
class Functor1 f where
    fmap1 :: (a -> b) -> f a -> f b
    -- fmap1 is a function of normal type a -> b which takes an input of the type f a and can produce the generalized functor type result. 


instance Functor1 ((->) a) where
    fmap1 g k = g . k 

    -- fmap (g :: r -> m)  (k :: a -> r) =  (k . g :: a -> m)  
    -- Instance of the fmap will have a type 
    -- :t (fmap) = (r -> m) -> (a -> r) -> (a -> m)
    -- You provide a function of type (r -> m) and in the partially applied function type you get (a -> r) as your f a element. 
    -- The existing library function which allows us to acheive this is the composition function. 
