module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Text.Parsec.Pos
import System.Environment

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  -- get_token (Token p Program) = Just (Token p Program)
  get_token token@(Token p Program) = Just token
  get_token _ = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token token@(Token p (Id x)) = Just token
  get_token _  = Nothing

varToken = tokenPrim show update_pos get_token where
  get_token token@(Token p Var) = Just token
  get_token _ = Nothing  

beginToken = tokenPrim show update_pos get_token where
  get_token token@(Token p Begin) = Just token
  get_token _ = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token token@(Token p End) = Just token
  get_token _ = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token token@(Token p SemiColon) = Just token
  get_token _ = Nothing

colonToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Colon) = Just token
  get_token _ = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Assign) = Just token
  get_token _ = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Int _)) = Just token
  get_token _ = Nothing

boolToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Bool _)) = Just token
  get_token _ = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ (Type x)) = Just token
  get_token _ = Nothing 

addToken = tokenPrim show update_pos get_token where
  get_token token@(Token _ Add) = Just token
  get_token _       = Nothing   

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos sp _ (Token (AlexPn _ line col) _ : _) = newPos (sourceName sp) line col
update_pos pos _ [] = pos

-- parsers para os não-terminais

program :: ParsecT [Token] [(Token,Token)] IO ([Token])
program = do
            a <- programToken 
            b <- idToken 
            c <- varToken
            d <- varDecl
            e <- beginToken 
            f <- stmts
            g <- endToken
            eof
            return (a:b:[c] ++ d++ [e] ++ f ++ [g])

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- idToken
            b <- colonToken
            c <- typeToken
            updateState(symtable_insert (a, get_default_value c))
            s <- getState
            liftIO (print s)
            return (a:b:[c])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
remaining_stmts = (do a <- semiColonToken
                      b <- assign
                      return (a:b)) <|> (return [])

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- expression
          s <- getState
          if (not (compatible (get_type a s) c)) then fail "type mismatch"
          else 
            do 
              updateState(symtable_update (a, c))
              s <- getState
              liftIO (print s)
              return (a:b:[c])

-- funções para verificação de tipos

get_default_value :: Token -> Token
get_default_value (Token p (Type "int")) = Token p (Int 0)
get_default_value (Token p (Type "bool")) = Token p (Bool False)

get_type :: Token -> [(Token, Token)] -> Token
get_type _ [] = error "variable not found"
get_type token@(Token _ (Id id1)) ((Token _ (Id id2), value):t) = 
         if id1 == id2 then value
         else get_type token t

compatible :: Token -> Token -> Bool
compatible (Token _ (Int _)) (Token _ (Int _)) = True
compatible (Token _ (Bool _)) (Token _ (Bool _)) = True
compatible _ _ = False

-- funções para o avaliador de expressões

expression :: ParsecT [Token] [(Token,Token)] IO(Token)
expression = try bin_expression <|> una_expression

una_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
una_expression = do
                   op <- addToken
                   a <- intToken 
                   return (a)
                 <|> 
                 do 
                   a <- boolToken
                   return (a)

--- funções considerando associatividade à esquerda                  
bin_expression :: ParsecT [Token] [(Token,Token)] IO(Token)
bin_expression = do
                   n1 <- intToken
                   result <- eval_remaining n1
                   return (result)

eval_remaining :: Token -> ParsecT [Token] [(Token,Token)] IO(Token)
eval_remaining n1 = do
                      op <- addToken
                      n2 <- intToken
                      result <- eval_remaining (eval n1 op n2)
                      return (result) 
                    <|> return (n1)                              

eval :: Token -> Token -> Token -> Token
eval (Token p1 (Int x)) (Token _ Add) (Token _ (Int y)) = 
      (Token p1 (Int (x + y)))

-- funções para a tabela de símbolos

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (token1@(Token p1 c1), v1) ((token2@(Token p2 c2), v2):t) = 
                               if c1 == c2 then (token1, v1) : t
                               else (token2, v2) : symtable_update (token1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (token1@(Token p1 c1), v1) ((token2@(Token p2 c2), v2):t) = 
                               if c1 == c2 then t
                               else (token2, v2) : symtable_remove (token1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = do
        args <- getArgs
        case args of 
          [fn] -> do
            tokens <- getTokens fn
            result <- parser tokens
            case result of
            { Left err -> print err; 
              Right ans -> print ans
            }
          _ -> putStrLn "Please inform the input filename. Closing application..."