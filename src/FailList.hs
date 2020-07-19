module FailList
( FailList (..)
, atIndex
, filter
, foldr
, head
) where

import Prelude hiding (filter, foldr, head)

data FailList e a = Fail e | Empty | Cons a (FailList e a)

atIndex :: (Integral i) => i -> FailList e a -> Maybe a
atIndex 0 (Cons x _) = Just x
atIndex n _ | n < 0 = Nothing
atIndex n (Cons _ xs) = atIndex (n - 1) xs
atIndex _ _ = Nothing

filter :: (a -> Bool) -> FailList e a -> FailList e a
filter _ Empty = Empty
filter _ (Fail err) = Fail err
filter predicate (Cons x xs) = if predicate x
                                  then Cons x (filter predicate xs)
                                  else filter predicate xs

foldr :: (a -> b -> b) -> b -> FailList e a -> Either e b
foldr _ _ (Fail err) = Left err
foldr _ z Empty = Right z
foldr f z (Cons x xs) = f x <$> foldr f z xs

head :: FailList e a -> Maybe a
head Empty = Nothing
head (Fail _) = Nothing
head (Cons x _) = Just x

instance Functor (FailList e) where
  fmap _ (Fail err) = Fail err
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance (Eq e, Eq a) => Eq (FailList e a) where
  Fail err == Fail otherErr = err == otherErr
  Empty == Empty = True
  Cons x xs == Cons y ys = x == y && xs == ys
  _ == _ = False
