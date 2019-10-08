-- Utilities for turning pseudoarrays into arrays
module FFI.Simple.PseudoArray
  ( class PseudoArray, Arguments, length, from, drop, slice --, unshift
  , Any, toAny, unsafeFromAny
  ) where

import FFI.Simple.Objects ( getProperty )
import Data.Function.Uncurried ( Fn2, runFn2, Fn3, runFn3 )

import Prelude (class Show)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Any :: Type

toAny :: forall a. a -> Any
toAny = unsafeCoerce

unsafeFromAny :: forall a. Any -> a
unsafeFromAny = unsafeCoerce

instance showAny :: Show Any where
  show _ = "Any"

foreign import data Arguments :: Type

class PseudoArray (array :: Type) (elt :: Type) | array -> elt

instance arrayPseudoArray     :: PseudoArray (Array e) e
instance argumentsPseudoArray :: PseudoArray Arguments Any

-- | Gets the length of a pseudoarray
length :: forall t a. PseudoArray t a => t -> Int
length = getProperty "length"

-- | Given a pseudoarray, turns it into a proper array
from :: forall t a. PseudoArray t a => t -> Array a
from = _from

foreign import _from :: forall t a. t -> Array a

-- | Given a pseudoarray, turns all but the first n items into a
-- | proper array
drop :: forall t a. PseudoArray t a => Int -> t -> Array a
drop = runFn2 _drop

foreign import _drop :: forall t a. Fn2 Int t (Array a)

-- | Given a pseudoarray, turns items n through m into a proper array
slice :: forall t a. PseudoArray t a => Int -> Int -> t -> Array a
slice = runFn3 _slice

foreign import _slice :: forall t a. Fn3 Int Int t (Array a)

-- TODO unshift :: forall t u a. PseudoArray t => PseudoArray u => t a -> u a -> Array a
--unshift :: forall t a. PseudoArray t => t a -> Array a -> Array a
--unshift = runFn2 _unshift

--foreign import _unshift :: forall t a. Fn2 (t a) (Array a) (Array a)
