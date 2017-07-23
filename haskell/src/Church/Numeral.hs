{-# LANGUAGE RankNTypes #-}

module Church.Numeral where

import Prelude (Show, show, ($), (+), (-), Int, (.), const, id, flip)

import Church.Bool

-- | Represent numbers as an iteration on some initial value
newtype Number = Number { count :: forall a. (a -> a) -> a -> a }

instance Show Number where
  show f = show $ count f (+1) 0

-- | A representation of natural numbers
data Nat = Z | S Nat deriving (Show)

-- | Nat is converted into a number by repeatedly
--   composing f with itself until you hit Z, where
--   zero is represented as const.
num :: Nat -> Number
num Z = Number $ flip const 
num (S n) = Number $ \f -> f . count (num n) f 

-- | When given the iteration function, addition is
--   the result of running f m times and then n times.
plus :: Number -> Number -> Number
plus (Number m) (Number n) = Number (\f -> n f . m f)

succ :: Number -> Number
succ = plus (num (S Z)) 

-- | Multiplication is the composition of two numbers
--   the type (a -> a) -> a -> a can be thought of as
--   (a -> a) -> (a -> a), so when composed we get n
--   applied m times.
mult :: Number -> Number -> Number
mult (Number m) (Number n) = Number (m . n)

exp :: Number -> Number -> Number
exp m n = Number $ count n (count m)

pred :: Number -> Number
pred (Number n) = Number $ \f x -> n (\g h -> h . g $ f) (const x) id

minus :: Number -> Number -> Number
minus m n = count n pred m

divide :: Number -> Number -> Number
divide n y = Number $ go (succ n) y
  where go n m f x =
          let d = minus n m
          in (truth . isZero) d (count d f x) (f (go d m f x))

isZero :: Number -> Bool
isZero n = count n (const false) true

leq :: Number -> Number -> Bool
leq m n = isZero (minus m n)

eq :: Number -> Number -> Bool
eq m n = and (leq m n) (leq n m)
