module Main (main) where

import Sigma.Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Text.Parsec.Pos
import System.Environment

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ []                                = pos

funToken :: ParsecT [Token] Env IO Token
funToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Fun) = Just token
  get_token _                   = Nothing

idToken :: ParsecT [Token] Env IO Token
idToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Id _)) = Just token
  get_token _                      = Nothing

lpToken :: ParsecT [Token] Env IO Token
lpToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ LP) = Just token
  get_token _                  = Nothing

rpToken :: ParsecT [Token] Env IO Token
rpToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ RP) = Just token
  get_token _                  = Nothing

lcbToken :: ParsecT [Token] Env IO Token
lcbToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ LCB) = Just token
  get_token _                   = Nothing

rcbToken :: ParsecT [Token] Env IO Token
rcbToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ RCB) = Just token
  get_token _                   = Nothing

semicolonToken :: ParsecT [Token] Env IO Token
semicolonToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Semicolon) = Just token
  get_token _                         = Nothing

colonToken :: ParsecT [Token] Env IO Token
colonToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Colon) = Just token
  get_token _                     = Nothing

assignToken :: ParsecT [Token] Env IO Token
assignToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Assign) = Just token
  get_token _                      = Nothing

printToken :: ParsecT [Token] Env IO Token
printToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Print) = Just token
  get_token _                     = Nothing

addToken :: ParsecT [Token] Env IO Token
addToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Add) = Just token
  get_token _                   = Nothing

subToken :: ParsecT [Token] Env IO Token
subToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Sub) = Just token
  get_token _                   = Nothing

multToken :: ParsecT [Token] Env IO Token
multToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Mult) = Just token
  get_token _                    = Nothing

divToken :: ParsecT [Token] Env IO Token
divToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Div) = Just token
  get_token _                   = Nothing

intLitToken :: ParsecT [Token] Env IO Token
intLitToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (IntLit _)) = Just token
  get_token _                          = Nothing

floatLitToken :: ParsecT [Token] Env IO Token
floatLitToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (FloatLit _)) = Just token
  get_token _                            = Nothing

typeToken :: ParsecT [Token] Env IO Token
typeToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ TInt)    = Just token
  get_token token@(Token _ TFloat)  = Just token
  get_token token@(Token _ TBool)   = Just token
  get_token token@(Token _ TString) = Just token
  get_token _                       = Nothing

-- ─────────────────────────────────────────────────────────────────────────────

data Value = VFloat Double | VString String
  deriving (Show, Eq)

-- Tabela de símbolos: lista de (nome, valor)
type Env = [(String, Value)]

env_insert :: String -> Value -> Env -> Env
env_insert name val env = env ++ [(name, val)]

env_update :: String -> Value -> Env -> Env
env_update name val []            = error ("variável não declarada: " ++ name)
env_update name val ((n,v):rest)
  | name == n = (name, val) : rest
  | otherwise = (n, v) : env_update name val rest

env_lookup :: String -> Env -> Value
env_lookup name []           = error ("variável não encontrada: " ++ name)
env_lookup name ((n,v):rest)
  | name == n = v
  | otherwise = env_lookup name rest

-- ─────────────────────────────────────────────────────────────────────────────

getId :: Token -> String
getId (Token _ (Id s)) = s
getId t                = error ("esperado Id, obteve: " ++ show t)

defaultValue :: Token -> Value
defaultValue (Token _ TInt)    = VFloat 0.0
defaultValue (Token _ TFloat)  = VFloat 0.0
defaultValue (Token _ TBool)   = VFloat 0.0
defaultValue (Token _ TString) = VString ""
defaultValue t                 = error ("tipo desconhecido: " ++ show t)

-- ──────────────────────────────── Gramática ─────────────────────────────────────

program :: ParsecT [Token] Env IO ()
program = do
  funToken
  name <- idToken
  lpToken
  rpToken
  lcbToken
  stmts
  rcbToken
  eof

stmts :: ParsecT [Token] Env IO ()
stmts = (do { stmt; stmts }) <|> return ()

stmt :: ParsecT [Token] Env IO ()
stmt = try printStmt <|> declAssignStmt

-- print ( expr ) ;
printStmt :: ParsecT [Token] Env IO ()
printStmt = do
  printToken
  lpToken
  val <- expr
  rpToken
  semicolonToken
  liftIO $ putStrLn $ showValue val

-- id : type = expr ;
declAssignStmt :: ParsecT [Token] Env IO ()
declAssignStmt = do
  nameToken <- idToken
  colonToken
  tyToken   <- typeToken
  assignToken
  val       <- expr
  semicolonToken
  let name = getId nameToken
  env <- getState
  let typedVal = coerce tyToken val
  if name `elem` map fst env
    then updateState (env_update name typedVal)
    else updateState (env_insert name typedVal)
  newEnv <- getState
  liftIO $ print newEnv

-- Coerce converte um VFloat para o tipo declarado (int trunca, float mantém)
coerce :: Token -> Value -> Value
coerce (Token _ TInt)   (VFloat d) = VFloat (fromIntegral (truncate d :: Int))
coerce (Token _ TFloat) v          = v
coerce _ v                         = v

-- ─────────────────────────────────────────────────────────────────────────────

expr :: ParsecT [Token] Env IO Value
expr = do
  t <- term
  exprRest t

exprRest :: Value -> ParsecT [Token] Env IO Value
exprRest acc =
  (do addToken; t <- term; exprRest (numOp (+) acc t))
  <|>
  (do subToken; t <- term; exprRest (numOp (-) acc t))
  <|>
  return acc

term :: ParsecT [Token] Env IO Value
term = do
  f <- factor
  termRest f

termRest :: Value -> ParsecT [Token] Env IO Value
termRest acc =
  (do multToken; f <- factor; termRest (numOp (*) acc f))
  <|>
  (do divToken;  f <- factor; termRest (numOp (/) acc f))
  <|>
  return acc

factor :: ParsecT [Token] Env IO Value
factor =
  -- ( expr )
  (do lpToken; v <- expr; rpToken; return v)
  <|>
  -- literal float
  (do Token _ (FloatLit d) <- floatLitToken; return (VFloat d))
  <|>
  -- literal int
  (do Token _ (IntLit i)   <- intLitToken;   return (VFloat (fromIntegral i)))
  <|>
  -- variável
  (do nameToken <- idToken
      env <- getState
      return (env_lookup (getId nameToken) env))

numOp :: (Double -> Double -> Double) -> Value -> Value -> Value
numOp op (VFloat a) (VFloat b) = VFloat (op a b)
numOp _ _ _                    = error "operação aritmética com tipo inválido"

-- Exibe um Value de forma legível
showValue :: Value -> String
showValue (VFloat d)
  | d == fromIntegral (round d :: Int) = show (round d :: Int)
  | otherwise                          = show d
showValue (VString s) = s

-- ─────────────────────────────────────────────────────────────────────────────

getTokens :: String -> IO [Token]
getTokens fn = do
  contents <- readFile fn
  return (alexScanTokens contents)

parser :: [Token] -> IO (Either ParseError ())
parser tokens = runParserT program [] "erro" tokens

main :: IO ()
main = do
  contents <- getContents
  let tokens = alexScanTokens contents
  result <- parser tokens
  case result of
    Left err -> print err
    Right () -> return ()