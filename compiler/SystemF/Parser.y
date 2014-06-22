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

"("    { OParen }
")"    { CParen }
"/\\"  { TLam }
"\\"   { Lam }
":"    { Colon }
forall { Forall }
"->"   { Arrow }
"."    { Dot }
let    { Let }
"="    { Eq }
in     { In }
fix    { Fix }
Int    { TypeInt }
if0    { If0 }
then   { Then }
else   { Else }
","    { Comma }
op3    { Op3 $$ }
op4    { Op4 $$ }
op6    { Op6 $$ }
op7    { Op7 $$ }
op11   { Op11 $$ }
op12   { Op12 $$ }
var    { LowId $$ }
tvar   { UpId $$ }
int    { Int $$ }
tupleField { TupleField $$ }

%right "in"
%right "->"
%nonassoc "else"

%left op5
%left op4
%left op3
%left op2
%left op1

%%

Exp : var  { \(tenv, env) -> FVar (fromJust (lookup $1 env)) }
    | "/\\" tvar "." Exp  { \(tenv, env) -> FBLam (\a -> $4 (($2, a):tenv, env)) }
    | "\\" "(" var ":" Typ ")" "." Exp  
        { \(tenv, env) -> FLam ($5 tenv) (\x -> $8 (tenv, ($3, x):env)) }
    | Exp Exp  { \(tenv, env) -> FApp  ($1 (tenv, env)) ($2 (tenv, env)) }
    -- Note that let x = e : T in f  rewrites to  (\(x : T) . f) e
    | let var "=" Exp ":" Typ in Exp  
        { \(tenv, env) -> FApp (FLam ($6 tenv) (\x -> $8 (tenv, ($2, x):env))) ($4 (tenv, env)) }
    | Exp Typ  { \(tenv, env) -> FTApp ($1 (tenv, env)) ($2 tenv) }
    | Exp op3  Exp  { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | Exp op4  Exp  { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | Exp op6  Exp  { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | Exp op7  Exp  { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | Exp op11 Exp  { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | Exp op12 Exp  { \e -> FPrimOp ($1 e) $2 ($3 e) }
    | int  { \_e -> FLit $1 }
    | if0 Exp then Exp else Exp  { \e -> Fif0 ($2 e) ($4 e) ($6 e) }
    | "(" Exps ")"  { \(tenv, env) -> FTuple ($2 (tenv, env)) }
    | Exp "." tupleField { \e -> FProj $3 ($1 e) } 
    | fix var "." "\\" "(" var ":" Typ ")" "." Exp ":" Typ 
        { \(tenv, env) -> 
            FFix ($8 tenv) (\y -> \x -> $11 (tenv, ($6, x):($2, y):env)) ($13 tenv) 
        }
    | "(" Exp ")"  { $2 }

Exps : Exp "," Exp   { \(tenv, env) -> ($1 (tenv, env):[$3 (tenv, env)]) }
     | Exp "," Exps  { \(tenv, env) -> ($1 (tenv, env):$3 (tenv, env)) }

-- data PFTyp t = FTVar t | FForall (t -> PFTyp t) | FFun (PFTyp t) (PFTyp t) | PFInt
Typ : tvar                 { \tenv -> FTVar (fromJust (lookup $1 tenv)) }
    | forall tvar "." Typ  { \tenv -> FForall (\a -> $4 (($2, a):tenv)) }
    | Typ "->" Typ  { \tenv -> FFun ($1 tenv) ($3 tenv) }
    | Int           { \_    -> PFInt }
    | "(" Typ ")"   { $2 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

reader :: String -> PFExp t e
reader = (\parser -> parser emptyEnvs) . parser . lexer
    where emptyEnvs = ([], [])

}
