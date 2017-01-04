{-# LANGUAGE InstanceSigs #-}

module State
  ( State(..)
  ) where

-- https://hackage.haskell.org/package/transformers-0.5.2.0/docs/src/Control.Monad.Trans.State.Lazy.html#State

-- |The State newtype represents a function from
-- one state to another.  This is my attempt at implementing
-- State.  Official library is here
-- https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html

newtype State s a =
  -- runState :: State s a -> s -> (a, s)
  State { runState :: s -> (a, s) }

instance Monad (State s) where
  return = pure

  -- m a -> (a -> m b) -> m b
  -- State s a -> (a -> State s b) -> State s b
  -- (s -> (a, s)) -> (a -> (s -> (b, s)) ) -> (s -> (b, s))
  (>>=) s f = State $ \ss ->
    let (a, b) = runState s ss
        (c, d) = runState (f a) b
    in  (c, d)


instance Applicative (State s) where
  pure a = State $ \s -> (a, s)

  -- f (a -> b) -> f a -> f b
  -- (State s (a -> b) -> State s a -> State s b
  -- (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
  (<*>) f a = State $ \ss ->

    --   a      s
    --   a -> b s
    let (c, d) = (runState a ss)
        (e, g) = (runState f d)
    in ((e c), g)


instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f m = State $ \ss ->
    let (a, b) = (runState m ss)
    in (f a, b)
