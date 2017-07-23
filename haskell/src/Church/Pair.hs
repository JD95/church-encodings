{-# LANGUAGE RankNTypes #-}

module Church.Pair where

newtype Pair a b = Pair (forall c. (a -> b -> c) -> c)

pair a b = Pair (\f -> f a b)

instance (Show a, Show b) => Show (Pair a b) where
  show (Pair p) = p (\x y -> "(" ++ show x ++ ", " ++ show y ++ ")")

