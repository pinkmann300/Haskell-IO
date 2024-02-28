class Functor1 f where
    fmap1 :: (a -> b) -> f a -> f b

instance Functor1 ((->) a) where
    fmap1 g k = g . k 

class Functor1 f => Applicative1 f where
    pure1 :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b 

instance Applicative1 ((->) a) where
    pure1 = const  
    gs <**> ps = \x -> gs x (ps x)



