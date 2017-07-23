{-# LANGUAGE RankNTypes #-}

module Church.Product where

import Prelude (Show, show, (++), const)

newtype Product a b = Product { prod :: forall c. (a -> b -> c) -> c }

pair a b = Product (\f -> f a b)

instance (Show a, Show b) => Show (Product a b) where
  show (Product p) = p (\x y -> "(" ++ show x ++ ", " ++ show y ++ ")")

fst :: Product a b -> a
fst (Product p) = p const

snd :: Product a b -> b
snd (Product p) = p (\_ y -> y)
