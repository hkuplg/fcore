{
module Language.HM.Parser where

import qualified Data.Map as Map

import Language.HM.Syntax
import Language.HM.Lexer
}

%name parser
%monad { P }
%tokentype  { Token }
%error      { parseError }

%token

    "("     { TkOParen }
    ")"     { TkCParen }
    "\\"    { TkLam }
    "."     { TkDot }
    "->"    { TkArrow }
    "let"   { TkLet }
    "rec"   { TkRec }
    "="     { TkEq }
    -- "and"   { TkAnd }
    "in"    { TkIn }
    "if"    { TkIf }
    "then"  { TkThen }
    "else"  { TkElse }
    ","     { TkComma }

    {- Terminals for types
    UPPERID { TkUpperId $$ }
    ":"     { TkColon }
    "forall" { TkForall }
    "Int"    { TkTyInt }
    "Bool"   { TkTyBool }
    -}

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
%nonassoc UMINUS UNEG

%%

exp
    : infixexp %prec EXP        { $1 }

infixexp
    : exp10                     { $1 }
    | infixexp "||" infixexp    { EBinOp Or  $1 $3 }
    | infixexp "&&" infixexp    { EBinOp And $1 $3 }
    | infixexp "==" infixexp    { EBinOp Eq  $1 $3 }
    | infixexp "!=" infixexp    { EBinOp Ne  $1 $3 }
    | infixexp "<"  infixexp    { EBinOp Lt  $1 $3 }
    | infixexp "<=" infixexp    { EBinOp Le  $1 $3 }
    | infixexp ">"  infixexp    { EBinOp Gt  $1 $3 }
    | infixexp ">=" infixexp    { EBinOp Ge  $1 $3 }
    | infixexp "+"  infixexp    { EBinOp Add $1 $3 }
    | infixexp "-"  infixexp    { EBinOp Sub $1 $3 }
    | infixexp "*"  infixexp    { EBinOp Mul $1 $3 }
    | infixexp "/"  infixexp    { EBinOp Div $1 $3 }
    | infixexp "%"  infixexp    { EBinOp Mod $1 $3 }

exp10
    : "\\" pat pats "->" exp                 { ELam ($2:$3) $5 }
    | "let" var pats "=" exp "in" exp        { ELet ($2, $3, $5) $7 }
    | "let" "rec" var pats "=" exp "in" exp  { ELetRec ($3, $4, $6) $8 }
    | "if" exp "then" exp "else" exp         { EIf $2 $4 $6 }
    | "-" aexp %prec UMINUS                  { EUnOp Neg $2 }
    | "!" aexp %prec UNEG                    { EUnOp Not $2 }
    | fexp                                   { $1 }

fexp
    : fexp aexp         { EApp $1 $2 }
    | aexp              { $1 }

aexp
    : var               { EVar $1 }
    | INTEGER           { ELit (LInteger $1) }
    | aexp "." UNDERID  { EProj $1 $3 }
    | "(" exp ")"       { $2 }
    | "(" tup_exprs ")" { ETup $2 }

tup_exprs
    : exp "," exp       { $1:[$3] }
    | exp "," tup_exprs { $1:$3   }

{- Type annotations are optional.
typ
    : "forall" tvar "." typ     { \tenv -> TForall (\a -> $4 (Map.insert $2 a tenv)) }
    -- Require an atyp on the LHS so that `for A. A -> A` cannot be
    -- parsed as `(for A. A) -> A` since `for A. A` is not a valid atyp.
    | atyp "->" typ             { \tenv -> TFun ($1 tenv) ($3 tenv) }
    | atyp                      { $1 }

atyp
    : tvar              { \tenv -> FTVar (fromMaybe (error $ "Not in scope: type variable: `" ++ $1 ++ "'") (Map.lookup $1 tenv)) }
    | "Int"             { \_    -> FInt }
    | "(" typ ")"       { $2 }

tvar : UPPERID       { $1 }
-}

pats
    : pat       { [$1] }
    | pat pats  { $1:$2 }

pat : var       { PVar $1 }

var  : LOWERID  { $1 }

{
data P a = Ok a | Error String deriving (Eq, Show)

instance Monad P where
    Ok x      >>= f = f x
    Error msg >>= f = Error msg
    return x        = Ok x

-- parseError :: [Token] -> P a
parseError tokens = Error ("Parse error before tokens:\n\t" ++ show tokens)

reader :: String -> P Exp
reader = parser . lexer
}
