
module Main where

import Lib
import Data.List

main :: IO ()
main = someFunc

add :: Integer -> Integer -> Integer
add a b =
  a + b

addOne :: Integer -> Integer

addOne a =
  add a 1

swap :: (a, b) -> (b, a)
swap (x, y) =
  (y, x)

class SemiGroup a where
  combine :: a -> a -> a

instance SemiGroup Integer where
  combine x y =
    add x y

instance SemiGroup Bool where
  combine x y =
    x || y

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
    add x y

checkAssociativity :: (Eq a, MyMonoid a) => a -> a -> a -> Bool
checkAssociativity x y z =
  myJoin x (myJoin y z) ==  myJoin (myJoin x y) z
