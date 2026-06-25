module Sigma.Types where

import Sigma.Lexer (Token)
import Text.Parsec (ParsecT)

data Value = VInt Int | VFloat Double | VString String
  deriving (Show, Eq)

type Env = [(String, Value)]

type SigmaParser a = ParsecT [Token] Env IO a
