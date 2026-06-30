module Main (main) where

import Sigma.Lexer (alexScanTokens)
import Sigma.Parser (program, installRuntime)
import Sigma.Environment (emptyEnv)
import Text.Parsec (runParserT, errorPos, sourceLine, sourceColumn, ParseError)
import Control.Exception (try, evaluate, fromException, SomeException, ErrorCall (..))
import Data.IORef (newIORef)
import System.Environment (getArgs)

sourceContext :: String -> Int -> Int -> String
sourceContext src ln col =
  let srcLines = lines src
      srcLine  = if ln >= 1 && ln <= length srcLines then srcLines !! (ln - 1) else ""
      caret    = replicate (col - 1) ' ' ++ "^"
  in srcLine ++ "\n" ++ caret

showParseError :: String -> ParseError -> String
showParseError src err =
  let pos = errorPos err
  in show err ++ "\n" ++ sourceContext src (sourceLine pos) (sourceColumn pos)

showRuntimeError :: String -> String -> String
showRuntimeError src msg =
  case extractPos msg of
    Just (ln, col) -> msg ++ "\n" ++ sourceContext src ln col
    Nothing        -> msg

extractPos :: String -> Maybe (Int, Int)
extractPos msg =
  let cleaned = map (\c -> if c `elem` ",()\":" then ' ' else c) msg
  in firstPos (words cleaned)
  where
    firstPos ("line" : l : "column" : c : _) =
      (,) <$> readMaybe l <*> readMaybe c
    firstPos (_ : rest) = firstPos rest
    firstPos []         = Nothing
    readMaybe s = case reads s of [(n, "")] -> Just n; _ -> Nothing

main :: IO ()
main = do
  args <- getArgs
  fn <- case args of
    [f] -> return f
    _   -> error "Use: sigma <file.sg>"
  contents <- readFile fn
  installRuntime
  envRef <- newIORef emptyEnv
  outcome <- try $ do
    let toks = alexScanTokens contents
    result <- runParserT (program envRef) emptyEnv fn toks
    evaluate result
  case outcome of
    Left e           -> putStrLn (showRuntimeError contents (exceptionMessage e))
    Right (Left err) -> putStrLn (showParseError contents err)
    Right (Right ()) -> return ()

exceptionMessage :: SomeException -> String
exceptionMessage e =
  case fromException e of
    Just (ErrorCallWithLocation msg _) -> msg
    Nothing                            -> show e
