module Tree where
data Space a = Space (a -> a -> Bool) (Tree a)

data Tree f a = Tip a | Space a (f a) (f Bool -> Space a)

insert :: a -> Space a -> Space a
insert a (Space comp t) = Space comp (go a t) where
  go a (Tip a) = Space 