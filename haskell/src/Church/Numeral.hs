{-# LANGUAGE RankNTypes, TypeFamilies, GADTs, MultiParamTypeClasses #-}

module Church.Numeral where

import Prelude (pure, Show, show, (+), (-), Int, (.))

import Church.Bool
import Church.SKI

-- | Represent numbers as an iteration on some initial value
newtype Number = Number { count :: forall a. (a -> a) -> a -> a }

instance Show Number where
  show f = show (count f (+1) 0)

-- | A representation of natural numbers
data Nat = Z | S Nat deriving (Show)

-- | Nat is converted into a number by repeatedly
--   composing f with itself until you hit Z, where
--   zero is represented as const.
num :: Nat -> Number
num Z = Number (flip k)
num (S n) = Number (\f -> f . count (num n) f)

-- |
-- When given the iteration function, addition is
-- the result of running f m times and then n times.
--
-- With normal lambdas we can implement this via:
--
-- > Number (\f -> n f . m f)
--
-- However, we want to implement this using only what
-- has been constructed so far.
--
-- To make this easier we can use the monad instance
-- of (->) to help guide us.
-- 
-- @
-- f :: (e -> b -> c) -> (e -> a -> b) -> (e -> a -> c)
-- f m n = do
--   a <- n
--   b <- m
--   pure (a . b)
-- @
--
-- This is similar to the reader monad. Imagine that there
-- is another input, e, which will be passed to both n and m
-- to give us a and b. We then just compose these functions.
-- Rewriting this in applicative form we get:
-- 
-- > f m n = pure (.) <*> m <*> n
--
-- It turns out that the (->) instance of applicative defines
-- pure as the k combinator and <*> as the s combinator, thus:
--
-- > f m n = pure (.) <*> m <*> n
-- > f m n = (<*>) ((<*>) (pure (.)) n) m
-- > f m n = s (s (k (.)) n) m
plus :: Number -> Number -> Number
plus (Number m) (Number n) = Number (s (s (k (.)) n) m)


succ :: Number -> Number
succ = plus (num (S Z)) 

-- | Multiplication is the composition of two numbers
--   the type (a -> a) -> a -> a can be thought of as
--   (a -> a) -> (a -> a), so when composed we get n
--   applied m times.
mult :: Number -> Number -> Number
mult (Number m) (Number n) = Number (m . n)

exp :: Number -> Number -> Number
exp m n = Number (count n (count m))

pred :: Number -> Number
pred (Number n) = Number (\f x -> n (\g h -> (h . g) f) (k x) i)

minus :: Number -> Number -> Number
minus m n = count n pred m

divide :: Number -> Number -> Number
divide n y = Number (go (succ n) y)
  where go n m f x =
          let d = minus n m
          in (truth . isZero) d (count d f x) (f (go d m f x))

isZero :: Number -> Bool
isZero n = count n (k false) true

leq :: Number -> Number -> Bool
leq m n = isZero (minus m n)

eq :: Number -> Number -> Bool
eq m n = and (leq m n) (leq n m)
