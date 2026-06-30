module Sigma.Types where

import Data.IORef (IORef)
import qualified Data.Map.Strict as M
import Sigma.Lexer (Token)
import Text.Parsec (ParsecT)

data Value
  = VInt Int
  | VFloat Double
  | VString String
  | VBool Bool
  | VArray [Value]
  | VMatrix [[Value]]
  | VStruct String [(String, Value)]
  | VTypeDef [(String, Token)]
  | VFunction [(String, Bool)] [Token] Env
  | VRef (IORef Value)
  | VVoid
  | VNull

instance Show Value where
  show (VInt i)         = "VInt " ++ show i
  show (VFloat d)       = "VFloat " ++ show d
  show (VString s)      = "VString " ++ show s
  show (VBool b)        = "VBool " ++ show b
  show (VArray vs)      = "VArray " ++ show vs
  show (VMatrix vs)     = "VMatrix " ++ show vs
  show (VStruct n fs)   = "VStruct " ++ show n ++ " " ++ show fs
  show (VTypeDef fs)    = "VTypeDef " ++ show fs
  show (VFunction ps body env) = "VFunction " ++ show ps ++ " " ++ show body ++ " " ++ show env
  show (VRef _)         = "VRef <cell>"
  show VVoid            = "VVoid"
  show VNull            = "VNull"

instance Eq Value where
  VInt a       == VInt b       = a == b
  VFloat a     == VFloat b     = a == b
  VString a    == VString b    = a == b
  VBool a      == VBool b      = a == b
  VArray a     == VArray b     = a == b
  VMatrix a    == VMatrix b    = a == b
  VStruct n fs == VStruct m gs = n == m && fs == gs
  VTypeDef a   == VTypeDef b   = a == b
  VNull        == VNull        = True
  _            == _            = False

type Scope = M.Map String Value

type Env = [Scope]

type SigmaParser a = ParsecT [Token] Env IO a
