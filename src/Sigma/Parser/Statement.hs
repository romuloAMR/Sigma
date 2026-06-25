module Sigma.Parser.Statement (stmts, program, funDecl) where

import Text.Parsec
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Sigma.Types
import Sigma.Lexer
import Sigma.Environment
import Sigma.Parser.Core
import Sigma.Parser.Expression

debug :: Bool
debug = True

debugEnv :: Env -> IO ()
debugEnv env = if debug then print env else return ()

runWithRef :: IORef Env -> [Token] -> SigmaParser () -> IO (Either ParseError ())
runWithRef envRef toks p = do
  env <- readIORef envRef
  result <- runParserT (do { r <- p; newEnv <- getState; liftIO (writeIORef envRef newEnv); return r }) env "sub" toks
  return result

paramGroup :: SigmaParser ()
paramGroup = do
  _ <- idToken
  _ <- many (try (do { _ <- commaToken; idToken }))
  _ <- colonToken
  _ <- returnTypeToken
  return ()

params :: SigmaParser ()
params =
  (do paramGroup
      _ <- many (try (do { _ <- commaToken; paramGroup }))
      return ())
  <|> return ()

funDecl :: IORef Env -> SigmaParser ()
funDecl envRef = do
  _ <- funToken
  _ <- idToken
  _ <- lpToken
  params
  _ <- rpToken
  _ <- colonToken
  _ <- returnTypeToken
  _ <- lcbToken
  stmts envRef
  _ <- rcbToken
  return ()

program :: IORef Env -> SigmaParser ()
program envRef = do
  _ <- many1 (funDecl envRef)
  eof

stmts :: IORef Env -> SigmaParser ()
stmts envRef = (do { stmt envRef; stmts envRef }) <|> return ()

stmt :: IORef Env -> SigmaParser ()
stmt envRef
    =  try (printStmt envRef)
   <|> try (whileStmt envRef)
   <|> try (ifStmt envRef)
   <|> try incrementStmt
   <|> try indexAssignStmt
   <|> try assignStmt
   <|> declAssignStmt

printStmt :: IORef Env -> SigmaParser ()
printStmt _ = do
  _ <- printToken
  _ <- lpToken
  val <- expr
  _ <- rpToken
  _ <- semicolonToken
  liftIO $ putStrLn $ showValue val

whileStmt :: IORef Env -> SigmaParser ()
whileStmt envRef = do
  _ <- whileToken
  _ <- lpToken
  condToks <- collectUntilRP
  _ <- rpToken
  _ <- lcbToken
  bodyToks <- collectBlock
  env <- getState
  liftIO $ writeIORef envRef env
  liftIO $ whileLoop envRef condToks bodyToks
  finalEnv <- liftIO $ readIORef envRef
  putState finalEnv

whileLoop :: IORef Env -> [Token] -> [Token] -> IO ()
whileLoop envRef condToks bodyToks = do
  env <- readIORef envRef
  condResult <- runParserT (do { c <- cond; return c }) env "cond" condToks
  case condResult of
    Left err -> error (show err)
    Right False -> return ()
    Right True  -> do
      result <- runWithRef envRef bodyToks (stmts envRef)
      case result of
        Left err -> error (show err)
        Right () -> whileLoop envRef condToks bodyToks

ifStmt :: IORef Env -> SigmaParser ()
ifStmt envRef = do
  _ <- ifToken
  _ <- lpToken
  c <- cond
  _ <- rpToken
  _ <- lcbToken
  if c
    then do
      stmts envRef
      _ <- rcbToken
      return ()
    else do
      _ <- collectBlock
      return ()

incrementStmt :: SigmaParser ()
incrementStmt = do
  nameToken <- idToken
  _ <- incToken
  _ <- semicolonToken
  let name = getId nameToken
  env <- getState
  let val = env_lookup name env
  let newVal = case val of
                 VInt i   -> VInt (i + 1)
                 VFloat v -> VFloat (v + 1)
                 _        -> error ("Não é possível incrementar o tipo: " ++ name)
  updateState (env_update name newVal)
  newEnv <- getState
  liftIO $ debugEnv newEnv

-- id[expr]...[expr] = expr ;
indexAssignStmt :: SigmaParser ()
indexAssignStmt = do
  nameToken <- idToken
  idxs <- many1 (do { _ <- mkTok LB; i <- expr; _ <- mkTok RB; return i })
  _ <- assignToken
  val <- expr
  _ <- semicolonToken
  let name = getId nameToken
  env <- getState
  let base = env_lookup name env
  let idxInts = map (\i -> case i of { VInt n -> n; VFloat n -> truncate n; _ -> error "index must be int" }) idxs
  let newBase = indexedUpdate base idxInts val
  updateState (env_update name newBase)
  newEnv <- getState
  liftIO $ debugEnv newEnv

assignStmt :: SigmaParser ()
assignStmt = do
  nameToken <- idToken
  _ <- assignToken
  val <- expr
  _ <- semicolonToken
  let name = getId nameToken
  env <- getState
  let _ = env_lookup name env
  updateState (env_update name val)
  newEnv <- getState
  liftIO $ debugEnv newEnv

declAssignStmt :: ParsecT [Token] Env IO ()
declAssignStmt = do
  pos <- getPosition
  nameToken <- idToken
  _ <- colonToken
  (tyToken, dims) <- typeAnnotation
  _ <- assignToken
  val       <- expr
  _ <- semicolonToken
  let name     = getId nameToken
  let typedVal = case dims of
                   0 -> coerce tyToken val
                   1 -> case val of { VArray _  -> val; _ -> error "expected array initializer" }
                   _ -> case val of { VMatrix _ -> val; _ -> error "expected matrix initializer" }
  env <- getState

  let typeMatch = case (tyToken, typedVal) of
                    (Token _ TInt,    VInt _)    -> True
                    (Token _ TFloat,  VFloat _)  -> True
                    (Token _ TString, VString _) -> True
                    (Token _ TBool,   VBool _)   -> True
                    (Token _ TInt,    VArray _)  -> True
                    (Token _ TFloat,  VArray _)  -> True
                    (Token _ TInt,    VMatrix _) -> True
                    (Token _ TFloat,  VMatrix _) -> True
                    _                            -> False

  if not typeMatch
    then do
      setPosition pos
      fail ("Type error: Type mismatch. Variable '" ++ name ++ "' cannot hold this type of value.")
    else
      if name `elem` map fst env
        then do
          setPosition pos
          fail ("Semantic error: variable '" ++ name ++ "' is declared in scope")
        else do
          updateState (env_insert name typedVal)
          newEnv <- getState
          liftIO $ debugEnv newEnv

