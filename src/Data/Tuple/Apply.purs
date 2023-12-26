module Data.Tuple.Apply where

import Prelude

import Control.Apply (lift2)
import Data.Tuple.Nested (type (/\), (/\))

both :: forall f a b. Applicative f => f a -> f b -> f (a /\ b)
both = lift2 (/\)

infixr 6 both as <&>
