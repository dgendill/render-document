{-# LANGUAGE QuasiQuotes #-}

module Parser where

import Document
import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ (r)

exEmptyLine :: String
exEmptyLine = "        \n"

exNonEmptyLine :: String
exNonEmptyLine = "        a"

emptyLine :: Parser Document
-- emptyLine = NL <$ (many (char ' ') <* (notFollowedBy anyChar))
emptyLine = NL <$ (many (char ' ') <* newline)

---

exIndent :: Int -> String
exIndent i = (take i $ repeat ' ') ++ "indented " ++ (show i)

anyIndentation :: Parser ([Document] -> Document)
anyIndentation =
  try ((indentation 0) <?> "No spaces")
  <|> (try (indentation 1) <?> "1 space")
  <|> (try (indentation 2) <?> "2 spaces")
  <|> (try (indentation 3) <?> "3 spaces")
  <|> (try (indentation 4) <?> "4 spaces")
  <|> (try (indentation 5) <?> "5 spaces")
  <|> (try (indentation 6) <?> "6 spaces")

indentation :: Int -> Parser ([Document] -> Document)
indentation i =
  ((I i) <$ ((count i (char ' ')) <* (notFollowedBy space)))
  <|> (I 0) <$ emptyLine

---

exDocText :: String
exDocText = "abc"

docText :: Parser Document
docText = do
  firstChar <- (noneOf " \n") <?> "not whitespace"
  -- text' <- (manyTill anyChar (try newline))
  -- text' <- (some anyChar)
  -- text' <- some (anyChar <* (notFollowedBy (char '\n')))
  text' <- some (noneOf "\n")

  return $ (T [firstChar : text'])

---

exDocument :: String
exDocument = [r|
TO WHOM IT MAY CONCERN
----------------------

  Good sir/madam,

    Be it known that a clever beast roams this
    area...

      It has been known to take the form of a
      kindly old woman, or a young boy with a
      bum leg.  It will try to trick you into
      helping it, and will jump upon you when you
      least expect it!

    Be cautious in these parts.

  Your friend,
    Sir Greg of the Horned Cabin on the North Face

|]

indentedText :: Parser Document
indentedText = do
  i <- anyIndentation
  m <- docText
  return $ (i [m])

document :: Parser Document
document = do
  i <- anyIndentation
  m <- many (
    (try emptyLine)
    <|> (try docText)
    <|> (try indentedText))
  return $ (i m)

---

testParser :: Parser a -> String -> Result a
testParser p s = parseString p mempty s

parserFailed :: Result a -> Bool
parserFailed r = case r of
  (Failure a) -> True
  (Success _) -> False

parserSuccess :: Result a -> Bool
parserSuccess = not.parserFailed

parserSuccessIs :: (Eq a) => a -> Result a -> Bool
parserSuccessIs s r = case r of
  (Success a) -> (a == s)
  (Failure b) -> False
