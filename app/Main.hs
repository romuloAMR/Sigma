module Main (main) where

import Sigma.Lexer (alexScanTokens)
import Sigma.Parser (program)
import Text.Parsec (runParserT, errorPos, sourceLine, sourceColumn, ParseError)
import Data.IORef (newIORef)
import System.Environment (getArgs)

showError :: String -> ParseError -> String
showError src err =
  let pos     = errorPos err
      ln      = sourceLine pos
      col     = sourceColumn pos
      srcLine = lines src !! (ln - 1)
      caret   = replicate (col - 1) ' ' ++ "^"
  in show err ++ "\n" ++ srcLine ++ "\n" ++ caret

main :: IO ()
main = do
  args <- getArgs
  fn <- case args of
    [f] -> return f
    _   -> error "Use: sigma <file.sg>"
  contents <- readFile fn
  let toks = alexScanTokens contents
  envRef <- newIORef []
  result <- runParserT (program envRef) [] fn toks
  case result of
    Left err -> putStrLn (showError contents err)
    Right () -> return ()
