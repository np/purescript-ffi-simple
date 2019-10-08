-- | Utilities for dealing with (uncurried) functions
module FFI.Simple.Functions
  ( new, bindTo, applyTo, delay, impure
  , (...), applyMethod, applyMethod'
  , args1, args2, args3, args4, args5
  , args6, args7, args8, args9, args10
  ) where

import Prelude ( class Monad, bind, flip, pure, (<<<), Unit, unit )
import FFI.Simple.Objects ( getProperty )
import FFI.Simple.PseudoArray ( Arguments )
import Data.Function.Uncurried
  ( Fn2, runFn2, Fn3, runFn3, Fn4,  runFn4
  , Fn5, runFn5, Fn6, runFn6, Fn7,  runFn7
  , Fn8, runFn8, Fn9, runFn9, Fn10, runFn10 )

-- | Call new on the function with an array or pseudoarray of arguments
new :: forall f a o. f -> a -> o
new = runFn2 _new

foreign import _new :: forall f a o. Fn2 f a o

bindTo :: forall f o. f -> o -> f
bindTo = runFn2 _bind

foreign import _bind :: forall f o. Fn2 f o f

-- | Apply a function to a this object with the given arguments
applyTo :: forall f this a b. f -> this -> a -> b
applyTo = runFn3 _apply

foreign import _apply :: forall f o a b. Fn3 f o a b

-- TODO: figure out what i should be
infixl 4 applyMethod' as ...

-- | Lookup and apply the method with the given name on the given
-- | object to the given this and an array or pseudoarray of arguments
applyMethod :: forall o a b. String -> o -> a -> b
applyMethod n o = applyTo (getProperty n o) o

-- | flip applyMethod
applyMethod' :: forall o a b. o -> String -> a -> b
applyMethod' = flip applyMethod

-- | `delay a m` is a delayed version of `m a`.
-- The pure computation of `m` becomes part of the effect.
-- For instance:
-- ```
-- test1 a m = do
--   let eff = delay a m
--   eff1
--   eff
--   eff2
--   eff
--
-- test2 a m = do
--   let eff = m a
--   eff1
--   eff
--   eff2
--   eff
-- ```
--
-- In `test1`, `m` is applied twice to `a`, after `eff1` and after `eff2`.
-- In `test2`, `m` is applied only once before `eff1`; yet the resulting effect
-- fires twice, after `eff1` and after `eff2`.
--
-- This is useful in two cases:
-- * The computation (not the effect) is expensive and one wants to perform it
--   if the effect needs to be done.
-- * The computation is impure and thus one wants to control its effect.
--
-- `delay a m` is equivalent to:
-- ```
-- do a' <- pure a
--    m a'
-- ```
delay :: forall a b m. Monad m => a -> (a -> m b) -> m b
delay = bind <<< pure

-- TODO: meant to be used on an Effect-like monad.
impure :: forall a m. Monad m => (Unit -> a) -> m a
impure f = do
  u <- pure unit
  pure (f u)

-- | returns an argument as an Arguments value
args1 :: forall a. a -> Arguments
args1 = _args1

foreign import _args1 :: forall a. a -> Arguments

-- | returns 2 arguments as an Arguments value
args2 :: forall a b. a -> b -> Arguments
args2 = runFn2 _args2

foreign import _args2 :: forall a b. Fn2 a b Arguments

-- | returns 3 arguments as an Arguments value
args3 :: forall a b c. a -> b -> c -> Arguments
args3 = runFn3 _args3

foreign import _args3 :: forall a b c. Fn3 a b c Arguments

-- | returns 4 arguments as an Arguments value
args4 :: forall a b c d. a -> b -> c -> d -> Arguments
args4 = runFn4 _args4

foreign import _args4 :: forall a b c d. Fn4 a b c d Arguments

-- | returns 5 arguments as an Arguments value
args5 :: forall a b c d e. a -> b -> c -> d -> e -> Arguments
args5 = runFn5 _args5

foreign import _args5 :: forall a b c d e. Fn5 a b c d e Arguments

-- | returns 6 arguments as an Arguments value
args6 :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Arguments
args6 = runFn6 _args6

foreign import _args6 :: forall a b c d e f. Fn6 a b c d e f Arguments

-- | returns 7 arguments as an Arguments value
args7 :: forall a b c d e f g. a -> b -> c -> d -> e -> f -> g -> Arguments
args7 = runFn7 _args7

foreign import _args7 :: forall a b c d e f g. Fn7 a b c d e f g Arguments

-- | returns 8 arguments as an Arguments value
args8 :: forall a b c d e f g h. a -> b -> c -> d -> e -> f -> g -> h -> Arguments
args8 = runFn8 _args8

foreign import _args8 :: forall a b c d e f g h. Fn8 a b c d e f g h Arguments

-- | returns 9 arguments as an Arguments value
args9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> Arguments
args9 = runFn9 _args9

foreign import _args9 :: forall a b c d e f g h i. Fn9 a b c d e f g h i Arguments

-- | returns 9 arguments as an Arguments value
args10 :: forall a b c d e f g h i j. a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Arguments
args10 = runFn10 _args10

foreign import _args10 :: forall a b c d e f g h i j. Fn10 a b c d e f g h i j Arguments
