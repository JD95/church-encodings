{-# LANGUAGE RankNTypes #-}

module Church.Maybe where

import Prelude (Show, show, (++), (.), flip, const)

newtype Maybe a = Maybe (forall c. (a -> c) -> c -> c)

instance Show a => Show (Maybe a) where
  show (Maybe m) = m (("Just " ++) . show) "Nothing"

just :: a -> Maybe a
just a = Maybe (\f _ -> f a)

nothing :: Maybe a
nothing = Maybe (flip const)
