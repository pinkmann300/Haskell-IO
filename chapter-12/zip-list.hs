newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where 
    fmap g (Z xs) = Z (fmap g xs)
    -- fmap :: (a -> b) -> Z [a] -> Z [b]

instance Applicative ZipList where
    pure x = (Z (repeat x))
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]
