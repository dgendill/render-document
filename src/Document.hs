{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Document
  ( TDocument(..)
  , Document
  , Indent
  , showDocumentStructure
  , exampleDocument1
  , exampleDocument2
  , indenter
  , indent
  ) where

import State
import Data.Tuple (swap)
import Control.Monad
import Control.Applicative (liftA2)
import Data.List (intersperse, groupBy)

-- | Indent is an alias for a State function that
-- goes from an Int (the depth of indentation) to a
-- new State of the same type.
type Indent = State Int String

-- | A Document is a tree of some type a, indentation
-- nodes, and new lines.
data TDocument a
  = I Int [TDocument a]
  | T [a]
  | NL

-- | A Document is a tree of text nodes (T), indentation
-- nodes, and new lines.
type Document = TDocument String

instance Show (TDocument String) where
  show NL = "\n"
  show (T s) = concatMap (++"\n") s
  show (I indentation doc) = fst $ runState (indent (concat $ map show doc)) indentation

instance (Eq a) => Eq (TDocument a) where
  (==) (I i1 d1) (I i2 d2) = (i1 == i2) && (d1 == d2)
  (==) (T v1) (T v2)       = v1 == v2
  (==) NL NL               = True
  (==) _  _                = False

showDocumentStructure :: (Show a) => TDocument a -> String
showDocumentStructure NL = "NL\n"
showDocumentStructure (T s) = "T [\n" ++ (concatMap (\n -> (show n) ++ "\n") s) ++ "]"
showDocumentStructure (I indentation doc) =
  ("I " ++ (show indentation)) ++ "\n" ++
  (fst $ runState (indent (concat $ map showDocumentStructure doc)) indentation)

printDocumentStructure :: Document -> IO ()
printDocumentStructure d = putStrLn $ showDocumentStructure d


-- sameIndentation :: Document -> Document -> Bool
-- sameIndentation (I i1 d1) (I i2 d2) = (i1 == i2)
-- sameIndentation (NL) (NL) = False
-- sameIndentation (T _) (T _) = False
-- sameIndentation _ _ = False

-- | Attempt to merge two structurally same documents.
-- mergeDocuments :: Document -> Document -> Either String Document
-- mergeDocuments a@(I i1 d1) b@(I i2 d2) =
--   if (i1 == i2)
--   then fmap (I i1) (zipWithM mergeDocuments d1 d2)
--   else Left "Documents do not have the same indentation."
-- mergeDocuments (NL) (NL) = Left "Newlines cannot be merged."
-- mergeDocuments (T a) (T b) = Right $ T (a ++ b)
-- mergeDocuments (I 0 a) b = fmap (I 0) (zipWithM mergeDocuments a [b])
-- mergeDocuments b (I 0 a) = fmap (I 0) (zipWithM mergeDocuments a [b])
-- mergeDocuments _ _ = Left "Could not merge documents."

exMergeDoc1 :: Document
exMergeDoc1 = I 4
  [ T [ "Ident 4."
      , "Ident 4."]
  ]

exMergeDoc2 :: Document
exMergeDoc2 = T [ "Ident 4." , "Ident 4."]

-- instance Monoid Document where
--   mempty = I 0 []
--   mappend a (I 0 []) = a
--   mappend (I 0 []) a = a
--   mappend a@(I i1 d1) b@(I i2 d2) =
--     if (i1 == i2)
--     then I i1 (d1 ++ d2)
--     else I i1 (d1 ++ [b])
--   mappend (I i1 d1) a = I i1 (d1 ++ [a])
--   mappend a (I i1 d1) = I i1 ([a] ++ d1)
--   mappend a b = I 0 [a, b]

-- simplifyDocument :: Document -> [[Document]]
-- simplifyDocument a@(I i1 d1) =
--   groupBy sameIndentation d1


-- | An example document structure.
exampleDocument2 = I 0
  [ T ["No indentation"]
  , I 4 [ T [ "Ident 4."
            , "Ident 4."]
        , I 7 [ T [ "Ident 7.  Total 11."]
              ]
        ]
  ]

-- | An example document structure.
exampleDocument1 = I 2
  [ NL
  , T ["Every man is the builder of a temple,"]
  , I 2
    [ T [ "called his body, to the god he worships,"
        , "after a style purely his own,"]
    , I 2
      [ T [ "we are all sculpters and painters, and our"
          , "material is our own flesh and blood \nand bones." ]
      , NL
      , T [ "- Henry David Thoreau"]
      ]
    ]
  ]

instance Foldable TDocument where
  foldr f a (NL) = a
  foldr f a (T (x:xs)) = foldr f (f x a) (T xs)
  foldr f a (T []) = a
  foldr f a (I i (x:xs)) = foldr f (foldr f a x) (I i xs)
  foldr f a (I i []) = a

testExampleDocumentFold :: IO ()
testExampleDocumentFold = do
  putStrLn $ foldr (\n a -> a ++ "\n" ++ n ++ "\n"  ) "" exampleDocument1


-- | Reads the indent size, and applies the
-- correct string padding to the accumulated
-- computation
indenter :: Indent
indenter = State $ \s ->
  let size = s
  in (take size $ repeat ' ', size)

-- | Accepts a string, and indents it based on the
-- current indentation state.
--
-- >>> runState (indent "Hello") 5
-- "     Hello"
--
indent :: String -> Indent
indent text = do
  p <- indenter
  let lines' = lines text
  return $ concat $ intersperse "\n" $ map (p++) lines'
