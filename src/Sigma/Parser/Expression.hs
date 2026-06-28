module Sigma.Parser.Expression (expr, cond, numOp, evalRelop, showValue) where

import Text.Parsec
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
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
numOp _ _ _                    = error "Type Error: Invalid data type in the mathematical operation"

evalDiv :: Value -> Value -> Value
evalDiv (VInt a) (VInt b) = VInt (a `div` b)
evalDiv a b               = numOp (/) a b

evalMod :: Value -> Value -> Value
evalMod (VInt a) (VInt b) = VInt (a `mod` b)
evalMod _ _ = error "Type error: The modulo operator (%) can only be used with integers"

evalUnaryMinus :: Value -> Value
evalUnaryMinus (VInt i)   = VInt (-i)
evalUnaryMinus (VFloat d) = VFloat (-d)
evalUnaryMinus _          = error "Type error: The modulo operator (-) can only be used with numbers"

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
evalRelop (Token _ Eq)  (VString a) (VString b) = a == b
evalRelop (Token _ NEq) (VString a) (VString b) = a /= b
evalRelop (Token _ Eq)  (VBool a) (VBool b) = a == b
evalRelop (Token _ NEq) (VBool a) (VBool b) = a /= b
evalRelop _ _ _ = error "Erro de Tipo: Operador relacional usado com tipo invalido"

showValue :: Value -> String
showValue (VInt i)    = show i
showValue (VFloat d)  = show d
showValue (VString s) = s
showValue (VBool b)   = if b then "true" else "false"
showValue (VArray vs)  = "[" ++ intercalate ", " (map showValue vs) ++ "]"
showValue (VMatrix vs) = intercalate "\n" (map showRow vs)
  where showRow row = "[" ++ intercalate ", " (map showValue row) ++ "]"

cond :: SigmaParser Bool
cond = do
  v <- expr
  case v of
    VBool b -> return b
    _       -> fail "Type error: The condition requires a Boolean expression"

expr :: SigmaParser Value
expr = do { t <- andExpr; orExprRest t }

orExprRest :: Value -> SigmaParser Value
orExprRest acc =
  (do _ <- orToken; t <- andExpr
      case (acc, t) of
        (VBool a, VBool b) -> orExprRest (VBool (a || b))
        _ -> fail "Type error: The 'or' operator requires two Boolean values"
  ) <|> return acc

andExpr :: SigmaParser Value
andExpr = do { f <- relExpr; andExprRest f }

andExprRest :: Value -> SigmaParser Value
andExprRest acc =
  (do _ <- andToken; f <- relExpr
      case (acc, f) of
        (VBool a, VBool b) -> andExprRest (VBool (a && b))
        _ -> fail "Type error: The 'and' operator requires two Boolean values"
  ) <|> return acc

relExpr :: SigmaParser Value
relExpr = try (do
  _ <- notToken
  v <- relExpr
  case v of
    VBool b -> return (VBool (not b))
    _ -> fail "Type error: The 'not' operator requires a Boolean value")
  <|> do
  left <- arithExpr
  (do
    op <- relopToken
    right <- arithExpr
    return (VBool (evalRelop op left right))
   ) <|> return left

arithExpr :: SigmaParser Value
arithExpr = do { t <- term; arithExprRest t }

arithExprRest :: Value -> SigmaParser Value
arithExprRest acc =
  (do _ <- addToken; t <- term
      let res = case (acc, t) of
                  (VString s1, VString s2) -> VString (s1 ++ s2)
                  (VString s1, v2)         -> VString (s1 ++ showValue v2)
                  (v1, VString s2)         -> VString (showValue v1 ++ s2)
                  _                        -> numOp (+) acc t
      arithExprRest res)
  <|> (do _ <- subToken; t <- term; arithExprRest (numOp (-) acc t))
  <|> return acc

term :: SigmaParser Value
term = do { p <- powerExpr; termRest p }

termRest :: Value -> SigmaParser Value
termRest acc =
  (do _ <- multToken; p <- powerExpr; termRest (numOp (*) acc p))
  <|> (do _ <- divToken; p <- powerExpr; termRest (evalDiv acc p))
  <|> (do _ <- modToken; p <- powerExpr; termRest (evalMod acc p))
  <|> return acc

powerExpr :: SigmaParser Value
powerExpr = do { f <- factor; powerExprRest f }

powerExprRest :: Value -> SigmaParser Value
powerExprRest acc =
  (do _ <- expToken
      p <- powerExpr
      return (numOp (**) acc p))
  <|> return acc

toIndex :: Value -> Int
toIndex (VInt i)   = i
toIndex (VFloat d) = truncate d
toIndex _          = error "Index error: The index must be an integer"

applyIndices :: Value -> [Value] -> Value
applyIndices v [] = v
applyIndices (VArray vs)  (i:rest) = applyIndices (vs !! toIndex i) rest
applyIndices (VMatrix vs) (i:rest) = applyIndices (VArray (vs !! toIndex i)) rest
applyIndices _ _ = error "Type error: cannot be indexed."

matrizBuiltin :: Token -> Maybe Token
matrizBuiltin tok@(Token _ (Id s))
  | s `elem` ["matrizFloat", "matrizInt", "matrizBool", "matrizString"] = Just tok
matrizBuiltin _ = Nothing

factor :: SigmaParser Value
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
  (do _ <- subToken
      v <- factor
      return (evalUnaryMinus v))
  <|>
  (do _ <- lpToken; v <- expr; _ <- rpToken; return v)
  <|>
  (do Token _ (FloatLit d) <- floatLitToken; return (VFloat d))
  <|>
  (do Token _ (StringLit s) <- tokenPrim show update_pos (\t -> case t of { Token _ (StringLit _) -> Just t; _ -> Nothing }); return (VString s))
  <|>
  (do Token _ (IntLit i) <- intLitToken; return (VInt i))
  <|>
  (do tok <- tokenPrim show update_pos matrizBuiltin
      _ <- lpToken; rows <- expr; _ <- commaToken; cols <- expr; _ <- rpToken
      let r = case rows of { VInt n -> n; VFloat n -> truncate n; _ -> error "Erro em Matriz: O numero de linhas deve ser inteiro" }
      let c = case cols of { VInt n -> n; VFloat n -> truncate n; _ -> error "Erro em Matriz: O numero de colunas deve ser inteiro" }
      let def = case tok of
                  Token _ (Id "matrizFloat")  -> VFloat 0.0
                  Token _ (Id "matrizInt")    -> VInt 0
                  Token _ (Id "matrizBool")   -> VBool False
                  Token _ (Id "matrizString") -> VString ""
                  _                           -> VFloat 0.0
      return (VMatrix (replicate r (replicate c def))))
  <|>
  (do nameToken <- idToken
      env <- getState
      let base = env_lookup (getId nameToken) env
      idxs <- many (do { _ <- mkTok LB; i <- expr; _ <- mkTok RB; return i })
      return (applyIndices base idxs))
  <|>
  (do Token _ (BoolLit b) <- boolLitToken; return (VBool b))

explicitCast :: Token -> Value -> Value
explicitCast (Token _ TInt)    v = toInt v
explicitCast (Token _ TFloat)  v = toFloat v
explicitCast (Token _ TString) v         = VString (showValue v)
explicitCast (Token _ TBool)   (VBool b) = VBool b
explicitCast (Token _ TBool)   (VInt i)  = VBool (i /= 0)
explicitCast _ v                         = v
