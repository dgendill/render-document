-- Tests not working right now.  need to figure out how to
-- compare two State types.

module Main where

import Test.QuickCheck (quickCheck, Arbitrary(..), frequency, Gen(..))
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Document (Indent)
import State (runState, State)

-- instance (Eq a, Eq b) => EqProp (State a b) where
--   (=-=) a b = (runState a 0) == (runState b 0)
--
-- instance (Arbitrary a, Arbitrary b) => Arbitrary (State a b) where
--   arbitrary = do
--     a <- arbitrary
--     b <- arbitrary
--     return $ State $ \s -> (a,b)
--
-- type Ints = (Int, Int, [Int])
-- type Strings = (String, String, [String])
--
main :: IO ()
main = do
  putStrLn "\nNo tests implemented yet."
--   quickBatch (functor ( undefined :: State Ints Strings))
--   quickBatch (applicative ( undefined :: State Ints Strings))
--   quickBatch (monad ( undefined :: State Ints Strings))
