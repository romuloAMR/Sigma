module Sigma.Parser.Core where

import Text.Parsec
import Text.Parsec.Pos
import Sigma.Lexer
import Sigma.Types

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ []                                = pos

mkTok :: TokenClass -> SigmaParser Token
mkTok tc = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ c) = if c == tc then Just tok else Nothing

funToken, lpToken, rpToken, lcbToken, rcbToken, semicolonToken, colonToken, assignToken, printToken, readToken, whileToken, ifToken, forToken, notToken, andToken, orToken, incToken, tintToken, addToken, subToken, multToken, divToken, modToken, expToken, commaToken :: SigmaParser Token
funToken       = mkTok Fun
lpToken        = mkTok LP
rpToken        = mkTok RP
lcbToken       = mkTok LCB
rcbToken       = mkTok RCB
semicolonToken = mkTok Semicolon
colonToken     = mkTok Colon
assignToken    = mkTok Assign
printToken     = mkTok Print
readToken      = mkTok Read
whileToken     = mkTok While
ifToken        = mkTok If
forToken       = mkTok For
notToken       = mkTok Not
andToken       = mkTok And
orToken        = mkTok Or
incToken       = mkTok Inc
tintToken      = mkTok TInt
addToken       = mkTok Add
subToken       = mkTok Sub
multToken      = mkTok Mult
divToken       = mkTok Div
modToken       = mkTok Mod
expToken       = mkTok Exp
commaToken     = mkTok Comma

idToken, intLitToken, floatLitToken, typeToken, returnTypeToken, relopToken, nextToken :: SigmaParser Token
idToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (Id _)) = Just tok
  get_tok _                    = Nothing

intLitToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (IntLit _)) = Just tok
  get_tok _                        = Nothing

floatLitToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (FloatLit _)) = Just tok
  get_tok _                          = Nothing

boolLitToken :: ParsecT [Token] Env IO Token
boolLitToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ (BoolLit _)) = Just tok
  get_tok _                         = Nothing

typeToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ TInt)    = Just tok
  get_tok tok@(Token _ TFloat)  = Just tok
  get_tok tok@(Token _ TBool)   = Just tok
  get_tok tok@(Token _ TString) = Just tok
  get_tok _                     = Nothing

returnTypeToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ TInt)    = Just tok
  get_tok tok@(Token _ TFloat)  = Just tok
  get_tok tok@(Token _ TBool)   = Just tok
  get_tok tok@(Token _ TString) = Just tok
  get_tok tok@(Token _ TNone)   = Just tok
  get_tok _                     = Nothing

relopToken = tokenPrim show update_pos get_tok where
  get_tok tok@(Token _ Ge)  = Just tok
  get_tok tok@(Token _ Le)  = Just tok
  get_tok tok@(Token _ Gt)  = Just tok
  get_tok tok@(Token _ Lt)  = Just tok
  get_tok tok@(Token _ Eq)  = Just tok
  get_tok tok@(Token _ NEq) = Just tok
  get_tok _                 = Nothing

nextToken = tokenPrim show update_pos Just

typeAnnotation :: SigmaParser (Token, Int)
typeAnnotation = do
  ty   <- typeToken
  dims <- many (try (do { _ <- mkTok LB; _ <- mkTok RB; return () }))
  return (ty, length dims)

collectBlock :: SigmaParser [Token]
collectBlock = go 0
  where
    go depth = do
      tok <- nextToken
      case tok of
        Token _ LCB -> do rest <- go (depth + 1); return (tok : rest)
        Token _ RCB
          | depth == 0 -> return []
          | otherwise  -> do rest <- go (depth - 1); return (tok : rest)
        _ -> do rest <- go depth; return (tok : rest)

collectUntilSemicolon :: SigmaParser [Token]
collectUntilSemicolon = do
  inp <- getInput
  case inp of
    [] -> return []
    (tok:_) -> case tok of
      Token _ Semicolon -> return []
      _ -> do _ <- nextToken; rest <- collectUntilSemicolon; return (tok:rest)

collectUntilRP :: SigmaParser [Token]
collectUntilRP = go 0
  where
    go depth = do
      inp <- getInput
      case inp of
        [] -> return []
        (tok:_) -> case tok of
          Token _ RP
            | depth == 0 -> return []
            | otherwise  -> do _ <- nextToken; rest <- go (depth-1); return (tok:rest)
          Token _ LP -> do _ <- nextToken; rest <- go (depth+1); return (tok:rest)
          _ -> do _ <- nextToken; rest <- go depth; return (tok:rest)
