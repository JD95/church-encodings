{-# LANGUAGE RankNTypes #-}

module Church.List where

import Prelude (Show, show, (++), flip, const, (.), ($))

import Church.Bool
import Church.Maybe

newtype List a = List (forall b. (a -> b -> b) -> b -> b)

instance Show a => Show (List a) where
  show (List t) = "[" ++ t build "" ++ "]"
       where build x "" = show x
             build x y = show x ++ ", " ++ y

nil :: List a
nil = List (flip const)

isNil :: List a -> Bool
isNil (List l) = l (\_ _ -> false) true

cons :: a -> List a -> List a
cons h (List t) = List  (\c n -> c h (t c n))

head :: List a -> Maybe a
head (List l) = l (const . just) nothing

tail :: List a -> List a
tail (List l) = List $ \c n -> l (\h t g -> g h (t c)) (const n) (flip const)

map :: (a -> b) -> List a -> List b
map f (List l) = l (\a b -> cons (f a) b) nil

append :: List a -> List a -> List a
append a (List b) = b cons a

