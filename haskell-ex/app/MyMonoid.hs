module MyMonoid where

class MyMonoid a where
  myId :: a

  myJoin :: a -> a -> a

instance MyMonoid Bool where
  myId = False

  myJoin x y =
    x || y

instance MyMonoid Integer where
  myId = 0

  myJoin x y =
    x + y
