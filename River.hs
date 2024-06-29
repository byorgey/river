{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module River where

-- XXX can we generalize so the repeating part has more than one element?
-- https://stackoverflow.com/questions/50144990/how-to-implement-an-eventually-repeating-list-in-haskell

import Prelude hiding (map, repeat, take, drop, (!!), zipWith, foldMap, and, or, all, any, maximum, minimum)
import Data.Monoid (All(..), Any(..))
import Data.Semigroup (Max(..), Min(..))

data River a = C a | Cons a !(River a)
  deriving (Functor)

expand :: River a -> River a
expand (C a) = Cons a (C a)
expand as = as

infixr 5 :::
pattern (:::) :: Eq a => a -> River a -> River a
pattern (:::) a as <- (expand -> Cons a as)
  where
    a' ::: C a | a' == a = C a
    a ::: as = Cons a as

{-# COMPLETE (:::) #-}

instance Eq a => Eq (River a) where
  C a == C b = a == b
  (a ::: as) == (b ::: bs) = a == b && as == bs

-- "zippy" Applicative instance
-- We don't use ::: to avoid Eq constraint
instance Applicative River where
  pure = C
  C f <*> C x = C (f x)
  C f <*> Cons x xs = Cons (f x) (C f <*> xs)
  Cons f fs <*> C x = Cons (f x) (fs <*> C x)
  Cons f fs <*> Cons x xs = Cons (f x) (fs <*> xs)

repeat :: a -> River a
repeat = C

(...) :: a -> River a
(...) = C

map :: (Eq a, Eq b) => (a -> b) -> River a -> River b
map f (C a) = C (f a)
map f (a ::: as) = f a ::: map f as

head :: Eq a => River a -> a
head (a ::: _) = a

tail :: Eq a => River a -> River a
tail (_ ::: as) = as

take :: Eq a => Int -> River a -> [a]
take n _ | n <= 0 = []
take n (a ::: as) = a : take (n-1) as

drop :: Eq a => Int -> River a -> River a
drop n s | n <= 0 = s
drop n (_ ::: as) = drop (n-1) as

(!!) :: Eq a => River a -> Int -> a
(a ::: _) !! 0 = a
C a !! _ = a
(_ ::: as) !! n = as !! (n-1)

zipWith :: (Eq a, Eq b, Eq c) => (a -> b -> c) -> River a -> River b -> River c
zipWith f (C a) (C b) = C (f a b)
zipWith f (a ::: as) (b ::: bs) = f a b ::: zipWith f as bs

normalize :: Eq a => River a -> River a
normalize (C a) = C a
normalize (a ::: as) = a ::: normalize as

instance (Show a, Eq a) => Show (River a) where
  show c = '[' : go (normalize c)
    where
      go :: River a -> String
      go (C a) = show a ++ "...]"
      go (a ::: as) = show a ++ "," ++ go as

class Semigroup m => IdempotentSemigroup m

instance IdempotentSemigroup All
instance IdempotentSemigroup Any
instance Ord a => IdempotentSemigroup (Max a)
instance Ord a => IdempotentSemigroup (Min a)

-- Can do stuff from Foldable that uses an idempotent monoid.  Can't
-- make Foldable instance because of Eq constraint (and it wouldn't
-- really be lawful anyway).
foldMap :: (Eq a, IdempotentSemigroup m) => (a -> m) -> River a -> m
foldMap f (C a) = f a
foldMap f (a ::: as) = f a <> foldMap f as

and :: River Bool -> Bool
and = getAll . foldMap All

or :: River Bool -> Bool
or = getAny . foldMap Any

all :: (a -> Bool) -> River a -> Bool
all f = and . fmap f

any :: (a -> Bool) -> River a -> Bool
any f = or . fmap f

maximum :: Ord a => River a -> a
maximum = getMax . foldMap Max

minimum :: Ord a => River a -> a
minimum = getMin . foldMap Min
