module Main where

import Sigma.Lexer

main :: IO ()
main = do
  input <- getContents
  print (alexScanTokens input)
