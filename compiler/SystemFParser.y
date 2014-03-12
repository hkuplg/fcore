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
forall { TokenForall }
Int    { TokenInt }
id     { TokenId $$ }

%left TokenArrow

%%

-- data PFExp t e = FVar e
--                | FBLam (t -> PFExp t e)
--                | FLam (PFTyp t) (e -> PFExp t e)
--                | FApp (PFExp t e) (PFExp t e)
--                | FTApp (PFExp t e) (PFTyp t)
Exp : id                                { \(_, env)    -> FVar (fromJust (lookup $1 env)) }
    | "/\\" id "." Exp                  { \(tenv, env) -> FBLam (\a -> $4 (($2, a):tenv, env)) }
    | "\\" "(" id ":" Typ ")" "." Exp   { \(tenv, env) -> FLam ($5 tenv) (\x -> $8 (tenv, ($3, x):env)) }
    | Exp Exp                           { \(tenv, env) -> FApp  ($1 (tenv, env)) ($2 (tenv, env)) }
    | Exp Typ                           { \(tenv, env) -> FTApp ($1 (tenv, env)) ($2 tenv) }
    | "(" Exp ")"                       { $2 }

-- data PFTyp t = FTVar t | FForall (t -> PFTyp t) | FFun (PFTyp t) (PFTyp t) | PFInt
Typ : id                { \tenv -> FTVar (fromJust (lookup $1 tenv)) }
    | forall id "." Typ { \tenv -> FForall (\a -> $4 (($2, a):tenv)) }
    | Typ "->" Typ      { \tenv -> FFun ($1 tenv) ($3 tenv) }
    | Int               { \_    -> PFInt }
    | "(" Typ ")"       { $2 }

{
parseError :: [SystemFToken] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

systemFRead :: String -> PFExp t e
systemFRead = (\parser -> parser emptyEnvs) . systemFParse . systemFLex
    where emptyEnvs = ([], [])

test1 :: String
test1 = "/\\a. \\(x:a) . x"

test2 :: String
test2 = "/\\a . (\\(f : a -> a) . \\(x : a) . f x) (idF a)"

test3 :: String
test3 = "/\\a . \\(x:a) . (idF a) x"

}
