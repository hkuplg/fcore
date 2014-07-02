{
module Language.HM.Parser where

import qualified Data.Map as Map 

import Language.HM.Syntax
import Language.HM.Lexer
}

%name parser
%tokentype  { Token }
%error      { parseError }

%token

"("     { TkOParen }
")"     { TkCParen }
"\\"    { TkLam }
"."     { TkDot }
"->"    { TkArrow }
":"     { TkColon }
"let"   { TkLet }
"rec"   { TkRec }
"="     { TkEq }
"and"   { TkAnd }
"in"    { TkIn }
"if"    { TkIf }
"then"  { TkThen }
"else"  { TkElse }
"forall" { TkForall }
"Int"    { TkTyInt }
"Bool"   { TkTyBool }
","     { TkComma }

"*"     { TkBinOp Mul }
"/"     { TkBinOp Div }
"%"     { TkBinOp Mod }
"+"     { TkBinOp Add }
"-"     { TkBinOp Sub }
"<"     { TkBinOp Lt }
"<="    { TkBinOp Le }
">"     { TkBinOp Gt }
">="    { TkBinOp Ge }
"=="    { TkBinOp Eq }
"!="    { TkBinOp Ne }
"&&"    { TkBinOp And }
"||"    { TkBinOp Or }

"!"     { TkUnOp Not }

INTEGER { TkInteger $$ }
LOWERID { TkLowerId $$ }
UPPERID { TkUpperId $$ }
UNDERID { TkUnderId $$ }

-- Precedence and associativity directives
%nonassoc EXP

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
    : infixexp %prec EXP                { ExpAnnot $1 Nothing }
    | infixexp ":" typ %prec EXP        { ExpAnnot $2 (Just $3) }

infixexp
    : exp10                     { $1 }
    | infixexp "*" exp10        { EBinOp $1 Mul $3 }
    | infixexp "/" exp10        { EBinOp $1 Div $3 }
    | infixexp "%" exp10        { EBinOp $1 Mod $3 }
    | infixexp "+" exp10        { EBinOp $1 Add $3 }
    | infixexp "-" exp10        { EBinOp $1 Sub $3 }
    | infixexp "<" exp10        { EBinOp $1 Lt  $3 }
    | infixexp "<=" exp10       { EBinOp $1 Le  $3 }
    | infixexp ">"  exp10       { EBinOp $1 Gt  $3 }
    | infixexp ">=" exp10       { EBinOp $1 Ge  $3 }
    | infixexp "==" exp10       { EBinOp $1 Eq  $3 }
    | infixexp "!=" exp10       { EBinOp $1 Ne  $3 }
    | infixexp "&&" exp10       { EBinOp $1 And $3 }
    | infixexp "||" exp10       { EBinOp $1 Or  $3 }

exp10 
    : "\\" var "->" exp                         { ELam $2 $4 }
    | "let" var pats "=" exp "in" exp           { ELet ($2, $3, $5) $7 }
    | "let" "rec" var pats "=" exp "in" exp     { ELetRec ($3, $4, $6) $8 }
    | "if" exp "then" exp "else" exp            { EIf $2 $4 $6 }
    | "-" INTEGER %prec UMINUS                  { ELit (LInteger (-$2)) }
    | fexp                                      { $1 }

fexp
    : fexp aexp         { EApp $1 $2 }
    | aexp              { $1 }

aexp   : aexp1          { $1 }

aexp1  : aexp2          { $1 }

aexp2 
    : var               { EVar $1 }
    | INTEGER           { ELit (LInteger $1) }
    | aexp "." UNDERID  { EProj $1 $3 } 
    | "(" exp ")"       { $2 }
    | "(" tup_exprs ")" { ETup $2 }

tup_exprs 
    : exp "," exp       { $1:[$3] }
    | exp "," tup_exprs { $1:$3   }

typ
    : "forall" tvar "." typ     { \tenv -> TForall (\a -> $4 (Map.insert $2 a tenv)) }
    -- Require an atyp on the LHS so that `for A. A -> A` cannot be
    -- parsed as `(for A. A) -> A` since `for A. A` is not a valid atyp.
    | atyp "->" typ             { \tenv -> TFun ($1 tenv) ($3 tenv) }
    | atyp                      { $1 }

atyp 
    : tvar              { \tenv -> FTVar (fromMaybe (error $ "Unbound type variable: `" ++ $1 ++ "'") (Map.lookup $1 tenv)) }
    | "Int"             { \_    -> FInt }
    | "(" typ ")"       { $2 }

pats 
    : pat               { [$1] }
    | pat pats          { $1:$2 }

pat 
    : var                       { PVar $1 }
    | "(" var ":" typ ")"       { PVarAnnot ($2, $4) }

var  : LOWERID       { $1 }
tvar : UPPERID       { $1 }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

reader :: String -> Exp
reader = parser . lexer
}
