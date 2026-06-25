module Sigma.Environment where

import Sigma.Types
import Sigma.Lexer (Token(..), TokenClass(..))

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

getId :: Token -> String
getId (Token _ (Id s)) = s
getId t                = error ("esperado Id, obteve: " ++ show t)

coerce :: Token -> Value -> Value
coerce (Token _ TInt)   (VFloat d) = VInt (truncate d)
coerce (Token _ TInt)   (VInt i)   = VInt i
coerce (Token _ TFloat) (VInt i)   = VFloat (fromIntegral i)
coerce (Token _ TFloat) v          = v
coerce _ v                         = v

toInt :: Value -> Value
toInt (VFloat d) = VInt (truncate d)
toInt (VInt i)   = VInt i
toInt v          = v
