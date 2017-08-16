{-# LANGUAGE RankNTypes #-}
module Church.SKI (s,k,i,fix, flip) where

import Prelude ()

iota f = f (\x y -> x) (\x y z -> x z (y z)) (\x y -> x)

s :: (e -> a -> b) -> (e -> a) -> (e -> b)
s = iota (iota iota)

k :: a -> b -> a
k = iota iota iota

i :: a -> a
i = s k k

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) = s (k s) k

--fix = let op = (k (s i i))
--        in s op (s (.) op)

fix f = let x = f x in x

flip :: (a -> b -> c) -> (b -> a -> c)
flip = s (k (s (k (s s (k k)))k)) s
