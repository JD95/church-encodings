{-# LANGUAGE RankNTypes, ExplicitForAll #-}

module Church.Bool where

import Prelude (Show, show, const, flip, ($))

-- | Booleans are functions which pick from one of two
--   inputs.
newtype Bool = Bool { truth :: forall a. a -> a -> a }

instance Show Bool where
  show b = truth b "True" "False"

-- | Always return the first input
true :: Bool
true = Bool const

-- | Always return the second input
false :: Bool
false = Bool (flip const)

-- | Both p and q must follow the true path
and :: Bool -> Bool -> Bool
and (Bool p) (Bool q) = Bool (p q p)

-- | If p failes then q could still follow true
or :: Bool -> Bool -> Bool
or (Bool p) (Bool q) = Bool (p p q)

-- | Invert the args for the bool function
not :: Bool -> Bool
not (Bool t) = Bool (flip t)

-- | Either a fails and b succeds or a succeeds and
--   b fails
xor :: Bool -> Bool -> Bool
xor (Bool a) b = Bool (a (truth $ not b) $ truth b)
