module Sigma.Parser.Expression (expr, cond, numOp, evalRelop, showValue) where

import Text.Parsec
import Control.Monad.IO.Class (liftIO)
import Sigma.Types
import Sigma.Lexer
import Sigma.Parser.Core
import Sigma.Environment

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

cond :: SigmaParser Bool
cond = do { t <- boolTerm; condRest t }

condRest :: Bool -> SigmaParser Bool
condRest acc =
  (do _ <- orToken; t <- boolTerm; condRest (acc || t))
  <|> return acc

boolTerm :: SigmaParser Bool
boolTerm = do { f <- boolFactor; boolTermRest f }

boolTermRest :: Bool -> SigmaParser Bool
boolTermRest acc =
  (do _ <- andToken; f <- boolFactor; boolTermRest (acc && f))
  <|> return acc

boolFactor :: SigmaParser Bool
boolFactor = do
  left  <- expr
  op    <- relopToken
  right <- expr
  return (evalRelop op left right)

expr :: SigmaParser Value
expr = do { t <- term; exprRest t }

exprRest :: Value -> SigmaParser Value
exprRest acc =
  (do _ <- addToken; t <- term; exprRest (numOp (+) acc t))
  <|> (do _ <- subToken; t <- term; exprRest (numOp (-) acc t))
  <|> return acc

term :: SigmaParser Value
term = do { f <- factor; termRest f }

termRest :: Value -> SigmaParser Value
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

explicitCast :: Token -> Value -> Value
explicitCast (Token _ TInt)    v            = toInt v
explicitCast (Token _ TFloat)  (VInt i)     = VFloat (fromIntegral i)
explicitCast (Token _ TFloat)  (VFloat d)   = VFloat d
explicitCast (Token _ TString) v            = VString (showValue v)
explicitCast (Token _ TBool)   (VBool b)    = VBool b
explicitCast (Token _ TBool)   (VInt i)     = VBool (i /= 0)
explicitCast _ v                            = v