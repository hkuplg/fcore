{
module SystemF.Parser where

import Data.Maybe       (fromJust)
import qualified Language.Java.Syntax as J (Op (..))

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

"*"      { PrimOp J.Mult   }
"/"      { PrimOp J.Div    }
"%"      { PrimOp J.Rem    }
"+"      { PrimOp J.Add    }
"-"      { PrimOp J.Sub    }
"<"      { PrimOp J.LThan  }
"<="     { PrimOp J.LThanE }
">"      { PrimOp J.GThan  }
">="     { PrimOp J.GThanE }
"=="     { PrimOp J.Equal  }
"!="     { PrimOp J.NotEq  }
"&&"     { PrimOp J.CAnd   }
"||"     { PrimOp J.COr    }

INT      { Int $$ }
UPPERID  { UpperId $$ }
LOWERID  { LowerId $$ }
UNDERID  { UnderId $$ }

-- Precedence and associativity directives
%right "in"
%right "->"
%nonassoc "else"

-- http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"

%nonassoc UMINUS

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
    | "-" INT %prec UMINUS                      { \e -> FLit (-$2) }
    | prim_op_exp       { $1 }
    | fexp              { $1 }

prim_op_exp 
    : exp "*" exp     { \e -> FPrimOp ($1 e) J.Mult   ($3 e) }
    | exp "/" exp     { \e -> FPrimOp ($1 e) J.Div    ($3 e) }
    | exp "%" exp     { \e -> FPrimOp ($1 e) J.Rem    ($3 e) }
    | exp "+" exp     { \e -> FPrimOp ($1 e) J.Add    ($3 e) }
    | exp "-" exp     { \e -> FPrimOp ($1 e) J.Sub    ($3 e) }
    | exp "<" exp     { \e -> FPrimOp ($1 e) J.LThan  ($3 e) }
    | exp "<=" exp    { \e -> FPrimOp ($1 e) J.LThanE ($3 e) }
    | exp ">"  exp    { \e -> FPrimOp ($1 e) J.GThan  ($3 e) }
    | exp ">=" exp    { \e -> FPrimOp ($1 e) J.GThanE ($3 e) }
    | exp "==" exp    { \e -> FPrimOp ($1 e) J.Equal  ($3 e) }
    | exp "!=" exp    { \e -> FPrimOp ($1 e) J.NotEq  ($3 e) }
    | exp "&&" exp    { \e -> FPrimOp ($1 e) J.CAnd   ($3 e) }
    | exp "||" exp    { \e -> FPrimOp ($1 e) J.COr    ($3 e) }

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
