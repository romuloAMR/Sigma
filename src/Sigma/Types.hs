module Sigma.Types where

import Sigma.Lexer (Token)
import Text.Parsec (ParsecT)
import qualified Data.Map.Strict as M

data Value = VInt Int | VFloat Double | VString String | VBool Bool | VArray [Value] | VMatrix [[Value]]
           | VStruct String [(String, Value)]
           | VTypeDef [(String, Token)]
  deriving (Show, Eq)

type Scope = M.Map String Value
type Env = [Scope]

type SigmaParser a = ParsecT [Token] Env IO a
