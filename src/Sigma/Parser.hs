module Main (main) where

import Sigma.Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Text.Parsec.Pos
import Data.IORef
import System.Environment

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ []                                = pos

mkTok :: TokenClass -> ParsecT [Token] Env IO Token
mkTok tc = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ c) = if c == tc then Just tok else Nothing

funToken       :: ParsecT [Token] Env IO Token
funToken       = mkTok Fun
lpToken        :: ParsecT [Token] Env IO Token
lpToken        = mkTok LP
rpToken        :: ParsecT [Token] Env IO Token
rpToken        = mkTok RP
lcbToken       :: ParsecT [Token] Env IO Token
lcbToken       = mkTok LCB
rcbToken       :: ParsecT [Token] Env IO Token
rcbToken       = mkTok RCB
semicolonToken :: ParsecT [Token] Env IO Token
semicolonToken = mkTok Semicolon
colonToken     :: ParsecT [Token] Env IO Token
colonToken     = mkTok Colon
assignToken    :: ParsecT [Token] Env IO Token
assignToken    = mkTok Assign
printToken     :: ParsecT [Token] Env IO Token
printToken     = mkTok Print
readToken      :: ParsecT [Token] Env IO Token
readToken      = mkTok Read
whileToken     :: ParsecT [Token] Env IO Token
whileToken     = mkTok While
ifToken        :: ParsecT [Token] Env IO Token
ifToken        = mkTok If
andToken       :: ParsecT [Token] Env IO Token
andToken       = mkTok And
orToken        :: ParsecT [Token] Env IO Token
orToken        = mkTok Or
incToken       :: ParsecT [Token] Env IO Token
incToken       = mkTok Inc
tintToken      :: ParsecT [Token] Env IO Token
tintToken      = mkTok TInt
addToken       :: ParsecT [Token] Env IO Token
addToken       = mkTok Add
subToken       :: ParsecT [Token] Env IO Token
subToken       = mkTok Sub
multToken      :: ParsecT [Token] Env IO Token
multToken      = mkTok Mult
divToken       :: ParsecT [Token] Env IO Token
divToken       = mkTok Div

idToken :: ParsecT [Token] Env IO Token
idToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (Id _)) = Just tok
  get_tok _                    = Nothing

intLitToken :: ParsecT [Token] Env IO Token
intLitToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (IntLit _)) = Just tok
  get_tok _                        = Nothing

floatLitToken :: ParsecT [Token] Env IO Token
floatLitToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (FloatLit _)) = Just tok
  get_tok _                          = Nothing

boolLitToken :: ParsecT [Token] Env IO Token
boolLitToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (BoolLit _)) = Just tok
  get_tok _                         = Nothing

typeToken :: ParsecT [Token] Env IO Token
typeToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ TInt)    = Just tok
  get_tok tok@(Token _ TFloat)  = Just tok
  get_tok tok@(Token _ TBool)   = Just tok
  get_tok tok@(Token _ TString) = Just tok
  get_tok _                     = Nothing

returnTypeToken :: ParsecT [Token] Env IO Token
returnTypeToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ TInt)    = Just tok
  get_tok tok@(Token _ TFloat)  = Just tok
  get_tok tok@(Token _ TBool)   = Just tok
  get_tok tok@(Token _ TString) = Just tok
  get_tok tok@(Token _ TNone)   = Just tok
  get_tok _                     = Nothing

relopToken :: ParsecT [Token] Env IO Token
relopToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ Ge)  = Just tok
  get_tok tok@(Token _ Le)  = Just tok
  get_tok tok@(Token _ Gt)  = Just tok
  get_tok tok@(Token _ Lt)  = Just tok
  get_tok tok@(Token _ Eq)  = Just tok
  get_tok tok@(Token _ NEq) = Just tok
  get_tok _                 = Nothing

nextToken :: ParsecT [Token] Env IO Token
nextToken = tokenPrim show update_pos Just

-- ─────────────────────────────────────────────────────────────────────────────

debug :: Bool
debug = True

debugEnv :: Env -> IO ()
debugEnv env = if debug then print env else return ()

data Value = VInt Int | VFloat Double | VString String | VBool Bool
  deriving (Show, Eq)

type Env = [(String, Value)]

env_insert :: String -> Value -> Env -> Env
env_insert name val env = env ++ [(name, val)]

env_update :: String -> Value -> Env -> Env
env_update name val [] = error ("Variable not declared: " ++ name)
env_update name val ((n,v):rest)
  | name == n = (name, val) : rest
  | otherwise = (n, v) : env_update name val rest

env_lookup :: String -> Env -> Value
env_lookup name [] = error ("Variable not found: " ++ name)
env_lookup name ((n,v):rest)
  | name == n = v
  | otherwise = env_lookup name rest

-- ────────────────────────────────────────────────────────────────────────────

getId :: Token -> String
getId (Token _ (Id s)) = s
getId t                = error ("esperado Id, obteve: " ++ show t)

coerce :: Token -> Value -> Value
coerce (Token _ TInt)   (VInt i)   = VInt i
coerce (Token _ TFloat) (VInt i)   = VFloat (fromIntegral i)
coerce (Token _ TFloat) v          = v
coerce _ v                         = v

explicitCast :: Token -> Value -> Value
explicitCast (Token _ TInt)    v            = toInt v
explicitCast (Token _ TFloat)  (VInt i)     = VFloat (fromIntegral i)
explicitCast (Token _ TFloat)  (VFloat d)   = VFloat d
explicitCast (Token _ TString) v            = VString (showValue v)
explicitCast (Token _ TBool)   (VBool b)    = VBool b
explicitCast (Token _ TBool)   (VInt i)     = VBool (i /= 0)
explicitCast _ v                            = v

toInt :: Value -> Value
toInt (VFloat d) = VInt (truncate d)
toInt (VInt i)   = VInt i
toInt v          = v

numOp :: (Double -> Double -> Double) -> Value -> Value -> Value
numOp op (VInt a)   (VInt b)   = let res = op (fromIntegral a) (fromIntegral b)
                                 in if res == fromIntegral (truncate res :: Int)
                                    then VInt (truncate res)
                                    else VFloat res
numOp op (VFloat a) (VFloat b) = VFloat (op a b)
numOp op (VInt a)   (VFloat b) = VFloat (op (fromIntegral a) b)
numOp op (VFloat a) (VInt b)   = VFloat (op a (fromIntegral b))
numOp _ _ _                    = error "Invalid data type in mathematical operation"

evalRelop :: Token -> Value -> Value -> Bool
evalRelop op (VInt a) (VInt b)     = evalRelop op (VFloat (fromIntegral a)) (VFloat (fromIntegral b))
evalRelop op (VInt a) (VFloat b)   = evalRelop op (VFloat (fromIntegral a)) (VFloat b)
evalRelop op (VFloat a) (VInt b)   = evalRelop op (VFloat a) (VFloat (fromIntegral b))
evalRelop (Token _ Ge)  (VFloat a) (VFloat b) = a >= b
evalRelop (Token _ Le)  (VFloat a) (VFloat b) = a <= b
evalRelop (Token _ Gt)  (VFloat a) (VFloat b) = a > b
evalRelop (Token _ Lt)  (VFloat a) (VFloat b) = a < b
evalRelop (Token _ Eq)  (VFloat a) (VFloat b) = a == b
evalRelop (Token _ NEq) (VFloat a) (VFloat b) = a /= b
evalRelop _ _ _ = error "Relational operator with invalid type"

showValue :: Value -> String
showValue (VInt i)   = show i
showValue (VFloat d)
  | d == fromIntegral (round d :: Int) = show (round d :: Int)
  | otherwise                          = show d
showValue (VString s) = s
showValue (VBool b)  = if b then "true" else "false"

-- ─────────────────────────────────────────────────────────────────
-- collect block tokens { ... }, without execute (while and if)

collectBlock :: ParsecT [Token] Env IO [Token]
collectBlock = go 0
  where
    go depth = do
      tok <- nextToken
      case tok of
        Token _ LCB -> do rest <- go (depth + 1); return (tok : rest)
        Token _ RCB
          | depth == 0 -> return []
          | otherwise  -> do rest <- go (depth - 1); return (tok : rest)
        _ -> do rest <- go depth; return (tok : rest)

-- collect until the closing ) (without consuming the ))

collectUntilRP :: ParsecT [Token] Env IO [Token]
collectUntilRP = go 0
  where
    go depth = do
      inp <- getInput
      case inp of
        [] -> return []
        (tok:_) -> case tok of
          Token _ RP
            | depth == 0 -> return []
            | otherwise  -> do _ <- nextToken; rest <- go (depth-1); return (tok:rest)
          Token _ LP -> do _ <- nextToken; rest <- go (depth+1); return (tok:rest)
          _ -> do _ <- nextToken; rest <- go depth; return (tok:rest)

-- ─────────────────────────────────────────────────────────────────────────────
-- execute a parser in generated's tokens, sharing env by IORef

runWithRef :: IORef Env -> [Token] -> ParsecT [Token] Env IO () -> IO (Either ParseError ())
runWithRef envRef toks p = do
  env <- readIORef envRef
  result <- runParserT (do { r <- p; newEnv <- getState; liftIO (writeIORef envRef newEnv); return r }) env "sub" toks
  return result

-- ─────────────────────────────────────────────────────────────────────────────
-- gramatics

commaToken :: ParsecT [Token] Env IO Token
commaToken = mkTok Comma

-- parse one param group: name1, name2, ..., nameN : type
-- e.g.  a, b, c: int   or just   x: float
paramGroup :: ParsecT [Token] Env IO ()
paramGroup = do
  _ <- idToken
  _ <- many (try (do { _ <- commaToken; idToken }))
  _ <- colonToken
  _ <- returnTypeToken
  return ()

-- params := ε | paramGroup (, paramGroup)*
params :: ParsecT [Token] Env IO ()
params =
  (do paramGroup
      _ <- many (try (do { _ <- commaToken; paramGroup }))
      return ())
  <|> return ()

funDecl :: IORef Env -> ParsecT [Token] Env IO ()
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

program :: IORef Env -> ParsecT [Token] Env IO ()
program envRef = do
  _ <- many1 (funDecl envRef)
  eof

stmts :: IORef Env -> ParsecT [Token] Env IO ()
stmts envRef = (do { stmt envRef; stmts envRef }) <|> return ()

stmt :: IORef Env -> ParsecT [Token] Env IO ()
stmt envRef
    =  try (printStmt envRef)
   <|> try (whileStmt envRef)
   <|> try (ifStmt envRef)
   <|> try incrementStmt
   <|> try assignStmt
   <|> declAssignStmt

-- print ( expr ) ;
printStmt :: IORef Env -> ParsecT [Token] Env IO ()
printStmt _ = do
  _ <- printToken
  _ <- lpToken
  val <- expr
  _ <- rpToken
  _ <- semicolonToken
  liftIO $ putStrLn $ showValue val

-- while ( cond ) { stmts }
-- stores tokens for code and body, executes in a loop via IORef.
whileStmt :: IORef Env -> ParsecT [Token] Env IO ()
whileStmt envRef = do
  _ <- whileToken
  _ <- lpToken
  condToks <- collectUntilRP
  _ <- rpToken
  _ <- lcbToken
  bodyToks <- collectBlock
  -- synchronizes the parsec environment with the IORef before the loop.
  env <- getState
  liftIO $ writeIORef envRef env
  liftIO $ whileLoop envRef condToks bodyToks
  -- resynchronizes after the loop.
  finalEnv <- liftIO $ readIORef envRef
  putState finalEnv

whileLoop :: IORef Env -> [Token] -> [Token] -> IO ()
whileLoop envRef condToks bodyToks = do
  env <- readIORef envRef
  -- condition available
  condResult <- runParserT (do { c <- cond; return c }) env "cond" condToks
  case condResult of
    Left err -> error (show err)
    Right False -> return ()
    Right True  -> do
      -- execute body
      result <- runWithRef envRef bodyToks (stmts envRef)
      case result of
        Left err -> error (show err)
        Right () -> whileLoop envRef condToks bodyToks

-- if ( cond ) { stmts }
ifStmt :: IORef Env -> ParsecT [Token] Env IO ()
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

-- id ++ ;
incrementStmt :: ParsecT [Token] Env IO ()
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

-- id = expr ;
assignStmt :: ParsecT [Token] Env IO ()
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

-- id : type = expr ;
declAssignStmt :: ParsecT [Token] Env IO ()
declAssignStmt = do
  pos <- getPosition
  nameToken <- idToken
  _ <- colonToken
  tyToken   <- typeToken
  _ <- assignToken
  val       <- expr
  _ <- semicolonToken
  let name     = getId nameToken
  let typedVal = coerce tyToken val
  env <- getState

  let typeMatch = case (tyToken, typedVal) of
                    (Token _ TInt,    VInt _)    -> True
                    (Token _ TFloat,  VFloat _)  -> True
                    (Token _ TString, VString _) -> True
                    (Token _ TBool,   VBool _)   -> True 
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

-- ─────────────────────────────────────────────────────────────────────────────
-- boolean conditions

cond :: ParsecT [Token] Env IO Bool
cond = do { t <- boolTerm; condRest t }

condRest :: Bool -> ParsecT [Token] Env IO Bool
condRest acc =
  (do _ <- orToken; t <- boolTerm; condRest (acc || t))
  <|> return acc

boolTerm :: ParsecT [Token] Env IO Bool
boolTerm = do { f <- boolFactor; boolTermRest f }

boolTermRest :: Bool -> ParsecT [Token] Env IO Bool
boolTermRest acc =
  (do _ <- andToken; f <- boolFactor; boolTermRest (acc && f))
  <|> return acc

boolFactor :: ParsecT [Token] Env IO Bool
boolFactor = do
  left  <- expr
  op    <- relopToken
  right <- expr
  return (evalRelop op left right)

-- ─────────────────────────────────────────────────────────────────────────────
-- arithimetic expressions

expr :: ParsecT [Token] Env IO Value
expr = do { t <- term; exprRest t }

exprRest :: Value -> ParsecT [Token] Env IO Value
exprRest acc =
  (do _ <- addToken; t <- term; exprRest (numOp (+) acc t))
  <|> (do _ <- subToken; t <- term; exprRest (numOp (-) acc t))
  <|> return acc

term :: ParsecT [Token] Env IO Value
term = do { f <- factor; termRest f }

termRest :: Value -> ParsecT [Token] Env IO Value
termRest acc =
  (do _ <- multToken; f <- factor; termRest (numOp (*) acc f))
  <|> (do _ <- divToken; f <- factor; termRest (numOp (/) acc f))
  <|> return acc

factor :: ParsecT [Token] Env IO Value
factor =
  (do tyTok <- typeToken   
      _     <- lpToken     
      v     <- expr        
      _     <- rpToken     
      return (explicitCast tyTok v))
  <|>
  (do _ <- readToken; _ <- lpToken; _ <- rpToken
      line <- liftIO getLine
      return (VFloat (read line)))
  <|>
  (do _ <- lpToken; v <- expr; _ <- rpToken; return v)
  <|>
  (do Token _ (FloatLit d) <- floatLitToken; return (VFloat d))
  <|>
  (do Token _ (IntLit i) <- intLitToken; return (VInt i))
  <|>
  (do nameToken <- idToken
      env <- getState
      return (env_lookup (getId nameToken) env))
  <|>
  (do Token _ (BoolLit b) <- boolLitToken; return (VBool b))

-- entry

showError :: String -> ParseError -> String
showError src err =
  let pos    = errorPos err
      ln     = sourceLine pos
      col    = sourceColumn pos
      srcLine = lines src !! (ln - 1)
      caret  = replicate (col - 1) ' ' ++ "^"
  in show err ++ "\n" ++ srcLine ++ "\n" ++ caret

main :: IO ()
main = do
  args <- getArgs
  fn <- case args of
    [f] -> return f
    _   -> error "Uso: sigma <arquivo.sg>"
  contents <- readFile fn
  let toks = alexScanTokens contents
  envRef <- newIORef []
  result <- runParserT (program envRef) [] fn toks
  case result of
    Left err -> putStrLn (showError contents err)
    Right () -> return ()
