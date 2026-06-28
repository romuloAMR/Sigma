{
module Sigma.Lexer( Token(..), TokenClass(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$idchar = [$alpha $digit \_]

tokens :-

  -- espaços
  [\ \t\f\v\r\n]+                  ;

  -- comentários
  "//"[^\n]*                       ;

  -- símbolos
  ";"                              { \p s -> Token p Semicolon }
  ":"                              { \p s -> Token p Colon }
  ","                              { \p s -> Token p Comma }
  "."                              { \p s -> Token p Dot }
  "("                              { \p s -> Token p LP }
  ")"                              { \p s -> Token p RP }
  "["                              { \p s -> Token p LB }
  "]"                              { \p s -> Token p RB }
  "{"                              { \p s -> Token p LCB }
  "}"                              { \p s -> Token p RCB }

  -- operadores relacionais e atribuição
  "=="                             { \p s -> Token p Eq }
  "!="                             { \p s -> Token p NEq }
  "<="                             { \p s -> Token p Le }
  ">="                             { \p s -> Token p Ge }
  "++"                             { \p s -> Token p Inc }
  "**"                             { \p s -> Token p Exp }
  "<"                              { \p s -> Token p Lt }
  ">"                              { \p s -> Token p Gt }
  "="                              { \p s -> Token p Assign }

  -- operadores aritméticos
  "+"                              { \p s -> Token p Add }
  "-"                              { \p s -> Token p Sub }
  "*"                              { \p s -> Token p Mult }
  "/"                              { \p s -> Token p Div }
  "%"                              { \p s -> Token p Mod }

  -- literais estruturados
  \"[^\"]*\"                       { \p s -> Token p (StringLit (take (length s - 2) (drop 1 s))) }
  $digit+ "." $digit+              { \p s -> Token p (FloatLit (read s)) }
  $digit+                          { \p s -> Token p (IntLit (read s)) }
  
  -- identificadores
  $alpha $idchar*                  { \p s -> decidirPalavra p s }

{

data Token = Token AlexPosn TokenClass
  deriving (Show, Eq)

data TokenClass
  = Semicolon
  | Colon
  | Comma
  | Dot
  | LP
  | RP
  | LB
  | RB
  | LCB
  | RCB
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | Exp
  | Eq
  | NEq
  | Lt
  | Gt
  | Le
  | Ge
  | Assign
  | Not
  | And
  | Or
  | Fun
  | If
  | Else
  | While
  | For
  | Return
  | TType
  | Struct
  | Print
  | Read
  | Inc
  | TInt
  | TFloat
  | TBool
  | TString
  | TNone
  | IntLit Int
  | FloatLit Double
  | BoolLit Bool
  | StringLit String
  | Id String
  deriving (Show, Eq)

decidirPalavra :: AlexPosn -> String -> Token
decidirPalavra p s = Token p $ case s of
  "fun"    -> Fun
  "if"     -> If
  "else"   -> Else
  "while"  -> While
  "for"    -> For
  "return" -> Return
  "type"   -> TType
  "struct" -> Struct
  "print"  -> Print
  "read"   -> Read
  "int"    -> TInt
  "float"  -> TFloat
  "bool"   -> TBool
  "string" -> TString
  "none"   -> TNone
  "true"   -> BoolLit True
  "false"  -> BoolLit False
  "not"    -> Not
  "and"    -> And
  "or"     -> Or
  _        -> Id s

token_posn :: Token -> AlexPosn
token_posn (Token p _) = p
}
