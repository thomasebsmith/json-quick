module FailList
( FailList (..)
, foldr
) where

import Prelude hiding (foldr)

data FailList e a = Fail e | Empty | Cons a (FailList e a)

foldr :: (a -> b -> b) -> b -> FailList e a -> Either e b
foldr _ _ (Fail err) = Left err
foldr _ z Empty = Right z
foldr f z (Cons x xs) = f x <$> foldr f z xs

instance Functor (FailList e) where
  fmap _ (Fail err) = Fail err
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance (Eq e, Eq a) => Eq (FailList e a) where
  Fail err == Fail otherErr = err == otherErr
  Empty == Empty = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False
