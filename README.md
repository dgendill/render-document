# Render Document

This is an example library that renders a Document structure such as the
following into text...

```haskell
-- type Document with data constructors 'I', 'NL', and 'T'
example = I 2
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
```

Newlines and indentation are maintained, and nested content is rendered
at the parent's indentation level plus it's own.

Primarily, this is an example library for beginners, and was done as an
exercise.  It is usable, but I'm sure there are better options out there.

# Library Usage

As a library, you should be able to simply import `Document(..)` construct a
value with type Document and then turn it into a String using `show`.

```
>>> show example
"  \n  Every man is the builder of a temple,\n    called his body, to the god he worships,\n    after a style purely his own,\n      we are all sculpters and painters, and our\n      material is our own flesh and blood \n      and bones.\n      \n      - Henry David Thoreau"
```

# Documentation

You can generate local documentation by running `stack haddock.`

# Example

To see some example output by running...

```bash
stack build
stack exec render-document-test
```

Be sure and have [stack](https://docs.haskellstack.org/en/stable/README/) installed.

Output looks like this...

```
  Every man is the builder of a temple,
    called his body, to the god he worships,
    after a style purely his own,
      we are all sculpters and paints, and our
      material is our own flesh and blood
      and bones.

      - Henry David Thoreau

No indentation
  Ident 4.
  Ident 4.
         Ident 7.  Total 11.
```
