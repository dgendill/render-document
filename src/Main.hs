module Main where

import Document (Document(..), exampleDocument1, exampleDocument2)

main :: IO ()
main = do
  putStrLn $ show exampleDocument1
  putStrLn ""
  putStrLn $ show exampleDocument2
