{-# LANGUAGE RankNTypes, ExplicitForAll #-}

module Church.Bool where

import Prelude (Show, show, const, flip, ($))

newtype Bool = Bool { truth :: forall a. a -> a -> a }

instance Show Bool where
  show b = truth b "True" "False"

true :: Bool
true = Bool const

false :: Bool
false = Bool (flip const)

and :: Bool -> Bool -> Bool
and (Bool p) (Bool q) = Bool (p q p)

or :: Bool -> Bool -> Bool
or (Bool p) (Bool q) = Bool (p p q)

not :: Bool -> Bool
not (Bool t) = Bool (flip t)

xor :: Bool -> Bool -> Bool
xor a b = Bool (truth a (truth $ not b) $ truth b)
