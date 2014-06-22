{
module SystemF.Parser where

import Data.Maybe       (fromJust)

import SystemF.Syntax
import SystemF.Lexer    
}

%name parser
%tokentype  { Token }
%error      { parseError }

%token

"("      { OParen }
")"      { CParen }
"/\\"    { TLam }
"\\"     { Lam }
":"      { Colon }
"forall" { Forall }
"->"     { Arrow }
"."      { Dot }
"let"    { Let }
"="      { Eq }
"in"     { In }
"fix"    { Fix }
"Int"    { TypeInt }
"if0"    { If0 }
"then"   { Then }
"else"   { Else }
","      { Comma }
OP3      { Op3 $$ }
OP4      { Op4 $$ }
OP6      { Op6 $$ }
OP7      { Op7 $$ }
OP11     { Op11 $$ }
OP12     { Op12 $$ }
INT      { Int $$ }
UPPERID  { UpperId $$ }
LOWERID  { LowerId $$ }
UNDERID  { UnderId $$ }

%right "in"
%right "->"
%nonassoc "else"

%left OP12
%left OP11
%left OP7
%left OP6
%left OP4
%left OP3

%%

exp 
    : "/\\" tvar "." exp                        { \(tenv, env) -> FBLam (\a -> $4 (($2, a):tenv, env)) }
    | "\\" "(" var ":" sigma_typ ")" "." exp    { \(tenv, env) -> FLam ($5 tenv) (\x -> $8 (tenv, ($3, x):env)) }
    | "fix" var "." "\\" "(" var ":" sigma_typ ")" "." exp ":" tau_typ 
        { \(tenv, env) -> FFix ($8 tenv) (\y -> \x -> $11 (tenv, ($6, x):($2, y):env)) ($13 tenv) }

    -- Note that let x = e : T in f  rewrites to  (\(x : T) . f) e
    | "let" var "=" exp ":" sigma_typ "in" exp  
        { \(tenv, env) -> FApp (FLam ($6 tenv) (\x -> $8 (tenv, ($2, x):env))) ($4 (tenv, env)) }

    | "if0" exp "then" exp "else" exp           { \e -> Fif0 ($2 e) ($4 e) ($6 e) }
    | exp OP3  exp      { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | exp OP4  exp      { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | exp OP6  exp      { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | exp OP7  exp      { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | exp OP11 exp      { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | exp OP12 exp      { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | fexp              { $1 }

fexp
    : fexp aexp         { \(tenv, env) -> FApp  ($1 (tenv, env)) ($2 (tenv, env)) }
    | fexp tau_typ      { \(tenv, env) -> FTApp ($1 (tenv, env)) ($2 tenv) }
    | aexp              { $1 }

aexp
    : var               { \(tenv, env) -> FVar (fromJust (lookup $1 env)) }
    | INT               { \_e -> FLit $1 }
    | "(" tup_exprs ")" { \(tenv, env) -> FTuple ($2 (tenv, env)) }
    | aexp "." UNDERID  { \e -> FProj $3 ($1 e) } 
    | "(" exp ")"       { $2 }

tup_exprs 
    : exp "," exp       { \(tenv, env) -> ($1 (tenv, env):[$3 (tenv, env)]) }
    | exp "," tup_exprs { \(tenv, env) -> ($1 (tenv, env):$3 (tenv, env)) }

sigma_typ 
    : "forall" tvar "." sigma_typ       { \tenv -> FForall (\a -> $4 (($2, a):tenv)) }
    | tau_typ                           { $1 }

tau_typ 
    : tvar                      { \tenv -> FTVar (fromJust (lookup $1 tenv)) }
    | tau_typ "->" tau_typ      { \tenv -> FFun ($1 tenv) ($3 tenv) }
    | "Int"                     { \_    -> PFInt }

var  : LOWERID       { $1 }
tvar : UPPERID       { $1 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

reader :: String -> PFExp t e
reader = (\parser -> parser emptyEnvs) . parser . lexer
    where emptyEnvs = ([], [])

}
