module Sigma.Environment where

import qualified Data.Map.Strict as M
import Sigma.Lexer (Token (..), TokenClass (..))
import Sigma.Types

emptyEnv :: Env
emptyEnv = [M.empty]

pushScope :: Env -> Env
pushScope env = M.empty : env

popScope :: Env -> Env
popScope [] = []
popScope (_ : rest) = rest

env_insert :: String -> Value -> Env -> Env
env_insert name val [] = error "Error: No active scope"
env_insert name val (currentScope : rest) =
  (M.insert name val currentScope) : rest

env_lookup :: String -> Env -> Value
env_lookup name [] = error ("Semantic error: Variable not found: " ++ name)
env_lookup name (scope : rest) =
  case M.lookup name scope of
    Just val -> val
    Nothing -> env_lookup name rest

env_update :: String -> Value -> Env -> Env
env_update name val [] = error ("Semantic error: Undeclared variable: " ++ name)
env_update name val (scope : rest) =
  if M.member name scope
    then (M.insert name val scope) : rest
    else scope : env_update name val rest

typeKey :: String -> String
typeKey name = "@type:" ++ name

registerType :: String -> [(String, Token)] -> Env -> Env
registerType name fields env =
  case reverse env of
    (bottom : upper) -> reverse (M.insert (typeKey name) (VTypeDef fields) bottom : upper)
    [] -> error "Error: No active scope"

lookupType :: String -> Env -> Maybe [(String, Token)]
lookupType name env =
  case env_lookupMaybe (typeKey name) env of
    Just (VTypeDef fields) -> Just fields
    _ -> Nothing

funKey :: String -> String
funKey name = "@fun:" ++ name

registerFun :: String -> [String] -> [Token] -> Env -> Env
registerFun name params body env =
  case reverse env of
    (bottom : upper) -> reverse (M.insert (funKey name) (VFunction params body []) bottom : upper)
    [] -> error "Error: No active scope"

lookupFun :: String -> Env -> Maybe ([String], [Token], Env)
lookupFun name env =
  case env_lookupMaybe (funKey name) env of
    Just (VFunction params body staticLink) -> Just (params, body, staticLink)
    _ -> Nothing

globalScope :: Env -> Scope
globalScope env =
  case reverse env of
    (bottom : _) -> bottom
    [] -> M.empty

env_lookupMaybe :: String -> Env -> Maybe Value
env_lookupMaybe _ [] = Nothing
env_lookupMaybe name (scope : rest) =
  case M.lookup name scope of
    Just val -> Just val
    Nothing -> env_lookupMaybe name rest

getId :: Token -> String
getId (Token _ (Id s)) = s
getId t = error ("expected Id, obtained: " ++ show t)

coerce :: Token -> Value -> Value
coerce (Token _ TInt) (VInt i) = VInt i
coerce (Token _ TFloat) (VInt i) = VFloat (fromIntegral i)
coerce (Token _ TFloat) v = v
coerce _ v = v

toInt :: Value -> Value
toInt (VFloat d) = VInt (truncate d)
toInt (VInt i) = VInt i
toInt (VString s) = VInt (read s)
toInt v = v

toFloat :: Value -> Value
toFloat (VInt i) = VFloat (fromIntegral i)
toFloat (VFloat d) = VFloat d
toFloat (VString s) = VFloat (read s)
toFloat v = v

indexedUpdate :: Value -> [Int] -> Value -> Value
indexedUpdate _ [] newVal = newVal
indexedUpdate (VArray vs) (i : rest) newVal =
  VArray (take i vs ++ [indexedUpdate (vs !! i) rest newVal] ++ drop (i + 1) vs)
indexedUpdate (VMatrix vs) (i : rest) newVal =
  let row = VArray (vs !! i)
      VArray newRow = indexedUpdate row rest newVal
   in VMatrix (take i vs ++ [newRow] ++ drop (i + 1) vs)
indexedUpdate _ _ _ = error "value is not indexable"
