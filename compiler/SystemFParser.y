{
module SystemFParser where

import SystemFTokens
import SystemFLexer
import SystemFJava

import Data.Maybe       (fromJust)

}

%name systemFParse
%tokentype  { SystemFToken }
%error      { parseError }

%token

"/\\"  { TokenTLambda }
"\\"   { TokenLambda }
"."    { TokenDot }
"->"   { TokenArrow }
":"    { TokenColon }
"("    { TokenOParen }
")"    { TokenCParen }
id     { TokenId $$ }

%%

Exp : "/\\" id "." Exp                  { \(tenv, env) -> FBLam (\a -> $4 (($2, a):tenv, env)) }
    | "\\" "(" id ":" id ")" "." Exp    { \(tenv, env) -> FLam PFInt (\x -> $8 (tenv, ($3, x):env)) }
    | id                                { \(tenv, env) -> FVar (fromJust (lookup $1 env)) }

{
parseError :: [SystemFToken] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

systemFRead :: String -> PFExp t e
systemFRead s = (systemFParse (systemFLex s)) ([], [])

test1 :: String
test1 = "/\\a. \\(x:a) . x"

test2 :: String
test2 = "/\\a . (\\(f : a -> a) . \\(x : a) . f x) (idF a)"

test3 :: String
test3 = "/\\a . \\(x:a) . (idF a) x"

}
