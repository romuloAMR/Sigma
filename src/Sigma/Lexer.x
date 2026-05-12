{
module Sigma.Lexer( Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$idchar = [$alpha $digit \_]

tokens :-

  -- espaços
  $white+                          ;

  -- comentários
  "//"[^\n]*                       ;

  -- símbolos
  ";"                              { \p s -> Semicolon p }
  ":"                              { \p s -> Colon p }
  ","                              { \p s -> Comma p }
  "."                              { \p s -> Dot p }
  "("                              { \p s -> LP p }
  ")"                              { \p s -> RP p }
  "["                              { \p s -> LB p }
  "]"                              { \p s -> RB p }
  "{"                              { \p s -> LCB p }
  "}"                              { \p s -> RCB p }

  -- operadores relacionais
  "=="                             { \p s -> Eq p }
  "!="                             { \p s -> NEq p }
  "<="                             { \p s -> Le p }
  ">="                             { \p s -> Ge p }
  "++"                             { \p s -> Inc p }
  "**"                             { \p s -> Exp p }
  "<"                              { \p s -> Lt p }
  ">"                              { \p s -> Gt p }
  "="                              { \p s -> Assign p }

  -- operadores lógicos e aritimeticos
  "+"                              { \p s -> Add p }
  "-"                              { \p s -> Sub p }
  "*"                              { \p s -> Mult p }
  "/"                              { \p s -> Div p }
  "not"                            { \p s -> Not p }
  "and"                            { \p s -> And p }
  "or"                             { \p s -> Or p }

  -- palavras reservadas
  "fun"                            { \p s -> Fun p }
  "if"                             { \p s -> If p }
  "while"                          { \p s -> While p }
  "for"                            { \p s -> For p }
  "return"                         { \p s -> Return p }
  "print"                          { \p s -> Print p }

  -- tipos
  "int"                            { \p s -> TInt p }
  "float"                          { \p s -> TFloat p }
  "bool"                           { \p s -> TBool p }
  "string"                         { \p s -> TString p }
  "none"                           { \p s -> TNone p }

  -- literais
  "true"                           { \p s -> BoolLit p True }
  "false"                          { \p s -> BoolLit p False }
  \"[^\"]*\"                       { \p s -> StringLit p (take (length s - 2) (drop 1 s)) }
  $digit+ "." $digit+              { \p s -> FloatLit p (read s) }
  $digit+                          { \p s -> IntLit p (read s) }
  
  -- identificadores
  $alpha $idchar*                  { \p s -> Id p s }

{
data Token
  = Semicolon AlexPosn
  | Colon AlexPosn
  | Comma AlexPosn
  | Dot AlexPosn
  | LP AlexPosn
  | RP AlexPosn
  | LB AlexPosn
  | RB AlexPosn
  | LCB AlexPosn
  | RCB AlexPosn
  | Add AlexPosn
  | Sub AlexPosn
  | Mult AlexPosn
  | Div AlexPosn
  | Exp AlexPosn
  | Eq AlexPosn
  | NEq AlexPosn
  | Lt AlexPosn
  | Gt AlexPosn
  | Le AlexPosn
  | Ge AlexPosn
  | Assign AlexPosn
  | Not AlexPosn
  | And AlexPosn
  | Or AlexPosn
  | Fun AlexPosn
  | If AlexPosn
  | While AlexPosn
  | For AlexPosn
  | Return AlexPosn
  | Print AlexPosn
  | Inc AlexPosn
  | TInt AlexPosn
  | TFloat AlexPosn
  | TBool AlexPosn
  | TString AlexPosn
  | TNone AlexPosn
  | IntLit AlexPosn Int
  | FloatLit AlexPosn Double
  | BoolLit AlexPosn Bool
  | StringLit AlexPosn String
  | Id AlexPosn String
  deriving (Show, Eq)

token_posn :: Token -> AlexPosn
token_posn token =
  case token of
    Semicolon p -> p
    Colon p -> p
    Comma p -> p
    Dot p -> p
    LP p -> p
    RP p -> p
    LB p -> p
    RB p -> p
    LCB p -> p
    RCB p -> p
    Add p -> p
    Sub p -> p
    Mult p -> p
    Div p -> p
    Exp p -> p
    Eq p -> p
    NEq p -> p
    Lt p -> p
    Gt p -> p
    Le p -> p
    Ge p -> p
    Assign p -> p
    Not p -> p
    And p -> p
    Or p -> p
    Fun p -> p
    If p -> p
    While p -> p
    For p -> p
    Return p -> p
    Print p -> p
    Inc p -> p
    TInt p -> p
    TFloat p -> p
    TBool p -> p
    TString p -> p
    TNone p -> p
    IntLit p _ -> p
    FloatLit p _ -> p
    BoolLit p _ -> p
    StringLit p _ -> p
    Id p _ -> p
}
