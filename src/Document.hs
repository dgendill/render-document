{-# LANGUAGE InstanceSigs #-}

module Document
  ( Document
  , Indent
  , exampleDocument1
  , exampleDocument2
  , indenter
  , indent
  ) where

import State
import Data.Tuple (swap)
import Control.Monad
import Data.List (intersperse)

-- | Indent is an alias for a State function that
-- goes from an Int (the depth of indentation) to a
-- new State of the same type.
type Indent = State Int String

-- | A Document is a tree of text nodes (T), indentation
-- nodes, and new lines.
data Document
  = I Int [Document]
  | T [String]
  | NL

instance Show Document where
  show NL = "\n"
  show (T s) = concatMap (++"\n") s
  show (I identation doc) = fst $ runState (indent (concat $ map show doc)) identation

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
