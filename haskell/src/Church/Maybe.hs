{-# LANGUAGE RankNTypes #-}

module Church.Maybe where

import Prelude (Show, show, (++), (.), flip, const)

-- | Maybe can be represented with the "maybe" function
newtype Maybe a = Maybe { maybe ::forall c. (a -> c) -> c -> c }

instance Show a => Show (Maybe a) where
  show (Maybe m) = m (("Just " ++) . show) "Nothing"

-- | When given a function and a default, apply the function
--   to the given value
just :: a -> Maybe a
just a = Maybe (\f _ -> f a)

-- | When given a function and a default, use the default
nothing :: Maybe a
nothing = Maybe (flip const)
