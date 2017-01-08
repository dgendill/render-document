-- Tests not working right now.  need to figure out how to
-- compare two State types.

module Main where

import Test.QuickCheck (quickCheck, Arbitrary(..), frequency, Gen(..))
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative
import Document (Indent, TDocument(..), Document)
import Parser
import Text.Trifecta (Result(..))
import Test.Hspec
import Control.Monad (liftM2, liftM)
import Data.Monoid ((<>))

-- instance (Eq a) => EqProp (TDocument a) where (=-=) = eq
-- --
instance Arbitrary a => Arbitrary (TDocument a) where
  arbitrary =
    frequency
      [ (2, liftM T arbitrary)
      , (1, return NL)
      , (5, liftM2 I arbitrary arbitrary)]

main :: IO ()
main = do
  -- quickBatch (monoid ( undefined :: Document))

  hspec $ do
    describe "emptyLine" $ do
      it "matches an empty line." $ do
        (testParser emptyLine exEmptyLine) `shouldSatisfy` parserSuccess

      it "does not match a non-empty line." $ do
        (testParser emptyLine exNonEmptyLine) `shouldSatisfy` parserFailed

    describe "indentation" $ do
      it "matches 0 space indentation" $ do
        (testParser (indentation 0) (exIndent 0)) `shouldSatisfy` parserSuccess

      it "matches 2 space indentation" $ do
        (testParser (indentation 2) (exIndent 2)) `shouldSatisfy` parserSuccess

      it "2 space indentation does not match 5 space indentation." $ do
        (testParser (indentation 2) (exIndent 5)) `shouldSatisfy` parserFailed

      it "anyIndentation matches 1 space of indentation" $ do
        (testParser (anyIndentation) (exIndent 1)) `shouldSatisfy` parserSuccess

      it "anyIndentation matches 4 spaces of indentation" $ do
        (testParser (anyIndentation) (exIndent 4)) `shouldSatisfy` parserSuccess


    describe "docText" $ do
      it "matches a line of text with no leading spaces" $ do
        (testParser docText exDocText) `shouldSatisfy` (parserSuccessIs (T ["abc"]))

      it "matches a line of text with no leading spaces (use indent)" $ do
        (testParser docText (exIndent 0)) `shouldSatisfy` parserSuccess

      it "it does not match a line of text with leading spaces" $ do
        (testParser docText (exIndent 2)) `shouldSatisfy` parserFailed

--property $ \x xs -> (testParser emptyLine exNonEmptyLine) `shouldBe` (Failure _)
--   quickBatch (functor ( undefined :: State Ints Strings))
--   quickBatch (applicative ( undefined :: State Ints Strings))
