-- myCurry :: ((x, y) -> z) -> x -> y -> z
--
-- myCurry f x y =
--   f (x, y)
--
-- myUnCurry :: (a -> b -> c) -> (a, b) -> c
--
-- myUnCurry f =
--   \(a, b) -> f a b
import MyMonoid

data Option a = None | Some a
  deriving Show

instance (MyMonoid a) => MyMonoid (Option a) where
  myId = None

  -- myJoin :: Option a -> Option a -> Option a
  myJoin (Some x) (Some y) = Some (x `myJoin` y)
  myJoin (Some x) None     = Some x
  myJoin None (Some y)     = Some y
  myJoin None None         = None

instance Functor Option where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (Some x) = Some (f x)
  fmap f None = None
