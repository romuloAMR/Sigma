module Sigma.Parser.Statement (stmts, program, funDecl) where

import Text.Parsec
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Sigma.Types
import Sigma.Lexer
import Sigma.Environment
import Sigma.Parser.Core
import Sigma.Parser.Expression
import qualified Data.Map.Strict as M

debug :: Bool
debug = True

debugEnv :: Env -> IO ()
debugEnv env = if debug then print env else return ()

runWithRef :: IORef Env -> [Token] -> SigmaParser () -> IO (Either ParseError ())
runWithRef envRef toks p = do
  env <- readIORef envRef
  result <- runParserT (do { r <- p; eof; newEnv <- getState; liftIO (writeIORef envRef newEnv); return r }) env "sub" toks
  return result

withNewScope :: SigmaParser a -> SigmaParser a
withNewScope action = do
  updateState pushScope
  result <- action
  updateState popScope
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

structField :: SigmaParser (String, Token)
structField = do
  nameToken <- idToken
  _ <- colonToken
  ty <- typeToken
  _ <- semicolonToken
  return (getId nameToken, ty)

typeDeclStmt :: IORef Env -> SigmaParser ()
typeDeclStmt _ = do
  _ <- typeKwToken
  nameToken <- idToken
  _ <- assignToken
  _ <- structToken
  _ <- lcbToken
  fields <- many1 structField
  _ <- rcbToken
  _ <- semicolonToken
  updateState (registerType (getId nameToken) fields)

topDecl :: IORef Env -> SigmaParser ()
topDecl envRef = try (typeDeclStmt envRef) <|> funDecl envRef

program :: IORef Env -> SigmaParser ()
program envRef = do
  _ <- many1 (topDecl envRef)
  eof

stmts :: IORef Env -> SigmaParser ()
stmts envRef = (do { stmt envRef; stmts envRef }) <|> return ()

stmt :: IORef Env -> SigmaParser ()
stmt envRef
    =  try (printStmt envRef)
   <|> try (whileStmt envRef)
   <|> try (forStmt envRef)
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
      modifyIORef envRef pushScope
      result <- runWithRef envRef bodyToks (stmts envRef)
      case result of
        Left err -> error (show err)
        Right () -> do
          modifyIORef envRef popScope
          whileLoop envRef condToks bodyToks

forStmt :: IORef Env -> SigmaParser ()
forStmt envRef = do
  _ <- forToken
  _ <- lpToken
  initToks <- collectUntilSemicolon
  _ <- semicolonToken
  condToks <- collectUntilSemicolon
  _ <- semicolonToken
  incrToks <- collectUntilRP
  _ <- rpToken
  _ <- lcbToken
  bodyToks <- collectBlock
  env <- getState
  liftIO $ writeIORef envRef env
  liftIO $ modifyIORef envRef pushScope
  
  let synSemi = case initToks of { (Token p _ : _) -> [Token p Semicolon]; [] -> [] }
  result <- liftIO $ runWithRef envRef (initToks ++ synSemi) declAssignStmt
  case result of
    Left err -> fail (show err)
    Right () -> liftIO $ forLoop envRef condToks incrToks bodyToks
  liftIO $ modifyIORef envRef popScope
  finalEnv <- liftIO $ readIORef envRef
  putState finalEnv

forLoop :: IORef Env -> [Token] -> [Token] -> [Token] -> IO ()
forLoop envRef condToks incrToks bodyToks = do
  env <- readIORef envRef
  condResult <- runParserT (do { c <- cond; return c }) env "cond" condToks
  case condResult of
    Left err -> error (show err)
    Right False -> return ()
    Right True -> do
      modifyIORef envRef pushScope
      bodyResult <- runWithRef envRef bodyToks (stmts envRef)
      case bodyResult of
        Left err -> error (show err)
        Right () -> do
          modifyIORef envRef popScope
          let synSemi = case incrToks of { (Token p _ : _) -> [Token p Semicolon]; [] -> [] }
          incrResult <- runWithRef envRef (incrToks ++ synSemi) incrementStmt
          case incrResult of
            Left err -> error (show err)
            Right () -> forLoop envRef condToks incrToks bodyToks

ifStmt :: IORef Env -> SigmaParser ()
ifStmt envRef = do
  _ <- ifToken
  _ <- lpToken
  c <- cond
  _ <- rpToken
  _ <- lcbToken
  if c
    then do
      withNewScope (stmts envRef)
      _ <- rcbToken
      _ <- optionMaybe (try (do { _ <- mkTok Else; _ <- lcbToken; toks <- collectBlock; return toks }))
      return ()
    else do
      _ <- collectBlock
      hasElse <- optionMaybe (try (do { _ <- mkTok Else; _ <- lcbToken; return () }))
      case hasElse of
        Nothing -> return ()
        Just _  -> do 
            withNewScope (stmts envRef)
            _ <- rcbToken
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
                 _        -> error ("It is not possible to increment the type: " ++ name)
  updateState (env_update name newVal)
  newEnv <- getState
  liftIO $ debugEnv newEnv

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
  let oldVal = env_lookup name env
  let typeMatch = case (oldVal, val) of
                    (VInt _, VInt _)       -> True
                    (VFloat _, VFloat _)   -> True
                    (VString _, VString _) -> True
                    (VBool _, VBool _)     -> True
                    (VArray _, VArray _)   -> True
                    (VMatrix _, VMatrix _) -> True
                    (VStruct a _, VStruct b _) -> a == b
                    _                      -> False
  
  if typeMatch then do
    updateState (env_update name val)
    newEnv <- getState
    liftIO $ debugEnv newEnv
  else
    fail ("Semantic Error: Incompatible type when assigning to the variable '" ++ name ++ "'")

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
                    (Token _ (Id t),  VStruct s _) -> t == s
                    _                            -> False

  let isDeclaredLocally = case env of
                            []        -> False
                            (scope:_) -> M.member name scope

  if not typeMatch
    then do
      setPosition pos
      fail ("Type error: Type mismatch. Variable '" ++ name ++ "' cannot hold this type of value.")
    else
      if isDeclaredLocally
        then do
          setPosition pos
          fail ("Semantic error: variable '" ++ name ++ "' is declared in scope")
        else do
          updateState (env_insert name typedVal)
          newEnv <- getState
          liftIO $ debugEnv newEnv
