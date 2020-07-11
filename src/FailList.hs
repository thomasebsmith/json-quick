module FailList
( FailList (..)
, foldr
) where

import Prelude hiding (foldr)

data FailList a = Fail | Empty | Cons a (FailList a)

foldr :: (a -> b -> b) -> b -> FailList a -> Maybe b
foldr _ _ Fail = Nothing
foldr _ z Empty = Just z
foldr f z (Cons x xs) = f x <$> foldr f z xs

instance Functor FailList where
  fmap _ Fail = Fail
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Eq a => Eq (FailList a) where
  Fail == Fail = True
  Empty == Empty = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False
