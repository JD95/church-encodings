{-# LANGUAGE RankNTypes #-}

module Church.CoProduct where

import Prelude ()

-- | Sum types can be represented via the "either" function
newtype CoProduct a b = CoProduct { coprod :: forall c. (a -> c) -> (b -> c) -> c }

-- | When given two functions, apply the first
--   one to the given input
left :: a -> CoProduct a b
left a = CoProduct (\f _ -> f a)

-- | When given two functions, apply the second
--   one to the given input
right :: b -> CoProduct a b
right b = CoProduct (\_ g -> g b)
