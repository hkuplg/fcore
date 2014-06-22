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

"("     { OParen }
")"     { CParen }
"/\\"   { TLam }
"\\"    { Lam }
":"     { Colon }
"forall" { Forall }
"->"    { Arrow }
"."     { Dot }
"let"   { Let }
"="     { Eq }
"in"    { In }
"fix"   { Fix }
"Int"   { typeInt }
"if0"   { If0 }
"then"  { Then }
"else"  { Else }
","     { Comma }
op3     { Op3 $$ }
op4     { Op4 $$ }
op6     { Op6 $$ }
op7     { Op7 $$ }
op11    { Op11 $$ }
op12    { Op12 $$ }
var     { LowId $$ }
tvar    { UpId $$ }
int     { Int $$ }
underId { UnderId $$ }

%right "in"
%right "->"
%nonassoc "else"

%left op12
%left op11
%left op7
%left op4
%left op3

%%

exp 
    : "/\\" tvar "." exp                { \(tenv, env) -> FBLam (\a -> $4 (($2, a):tenv, env)) }
    | "\\" "(" var ":" typ ")" "." exp  { \(tenv, env) -> FLam ($5 tenv) (\x -> $8 (tenv, ($3, x):env)) }
    | "fix" var "." "\\" "(" var ":" typ ")" "." exp ":" typ    { \(tenv, env) -> 
                                            FFix ($8 tenv) (\y -> \x -> $11 (tenv, ($6, x):($2, y):env)) ($13 tenv) }

    -- Note that let x = e : T in f  rewrites to  (\(x : T) . f) e
    | "let" var "=" exp ":" typ "in" exp  { \(tenv, env) -> 
                                            FApp (FLam ($6 tenv) (\x -> $8 (tenv, ($2, x):env))) ($4 (tenv, env)) }

    | "if0" exp "then" exp "else" exp   { \e -> Fif0 ($2 e) ($4 e) ($6 e) }
    | aexp op3  aexp                    { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | aexp op4  aexp                    { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | aexp op6  aexp                    { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | aexp op7  aexp                    { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | aexp op11 aexp                    { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | aexp op12 aexp                    { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | fexp                              { $1 }

fexp 
    : fexp aexp                 { \(tenv, env) -> FApp  ($1 (tenv, env)) ($2 (tenv, env)) }
    | fexp atyp                 { \(tenv, env) -> FTApp ($1 (tenv, env)) ($2 tenv) }
    | aexp                      { $1 }

aexp
    : int                       { \_e -> FLit $1 }
    | var                       { \(tenv, env) -> FVar (fromJust (lookup $1 env)) }
    | "(" tup_exprs ")"         { \(tenv, env) -> FTuple ($2 (tenv, env)) }
    | aexp "." underId          { \e -> FProj $3 ($1 e) } 

tup_exprs 
    : exp "," exp               { \e -> ($1 e:[$3 e]) }
    | exp "," tup_exprs         { \e -> ($1 e:$3 e) }

typ 
    : tvar                      { \tenv -> FTVar (fromJust (lookup $1 tenv)) }
    | "forall" tvar "." typ     { \tenv -> FForall (\a -> $4 (($2, a):tenv)) }
    | atyp                      { $1 }

atyp 
    : "Int"                     { \_    -> PFInt }
    | atyp "->" atyp            { \tenv -> FFun ($1 tenv) ($3 tenv) }
    | "(" typ ")"               { $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

reader :: String -> PFExp t e
reader = (\parser -> parser emptyEnvs) . parser . lexer where emptyEnvs = ([], [])

}
