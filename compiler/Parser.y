{
{-# LANGUAGE RecordWildCards #-}

module Parser where

import qualified Language.Java.Syntax as J (Op (..))

import Src
import Lexer

import JavaUtils
}

%name parseExpr expr
%tokentype { Token }
%monad     { P }
%error     { parseError }

%token

  "("      { Toparen }
  ")"      { Tcparen }
  "["      { Tobrack }
  "]"      { Tcbrack }
  "::"     { Tdcolon }
  "{"      { Tocurly }
  "}"      { Tccurly }
  "/\\"    { Ttlam }
  "\\"     { Tlam }
  ":"      { Tcolon }
  ";"      { Tsemi }
  "forall" { Tforall }
  "->"     { Tarrow }
  "."      { Tdot }
  "&"      { Tandtype }
  ",,"     { Tmerge }
  "with"   { Twith }
  "this"   { Tthis }
  "super"  { Tsuper }
  "let"    { Tlet }
  "rec"    { Trec }
  "="      { Teq }
  "and"    { Tand }
  "in"     { Tin }
  "if"     { Tif }
  "then"   { Tthen }
  "else"   { Telse }
  ","      { Tcomma }


  UPPERID  { Tupperid $$ }
  LOWERID  { Tlowerid $$ }
  UNDERID  { Tunderid $$ }

  JAVACLASS { Tjavaclass $$ }
  "new"     { Tnew }

  "module"  { Tmodule }
  "end"     { Tend }

  INT      { Tint $$ }
  STRING   { Tstring $$ }
  BOOL     { Tbool $$ }
  CHAR     { Tchar $$ }
  "()"     { Tunitlit }
  "Unit"   { Tunit }

  "*"      { Tprimop J.Mult   }
  "/"      { Tprimop J.Div    }
  "%"      { Tprimop J.Rem    }
  "+"      { Tprimop J.Add    }
  "-"      { Tprimop J.Sub    }
  "<"      { Tprimop J.LThan  }
  "<="     { Tprimop J.LThanE }
  ">"      { Tprimop J.GThan  }
  ">="     { Tprimop J.GThanE }
  "=="     { Tprimop J.Equal  }
  "!="     { Tprimop J.NotEq  }
  "&&"     { Tprimop J.CAnd   }
  "||"     { Tprimop J.COr    }

-- Precedence and associativity directives
%nonassoc EOF

%right "in"
%right "->"
%nonassoc "else"

-- http://en.wikipedia.org/wiki/Order_of_operations#Programming_languages
%left ",,"
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%nonassoc UMINUS

%%

-- Note: There are times when it is more convenient to parse a more general
-- language than that which is actually intended, and check it later.

-- The parser rules of GHC may come in handy:
-- https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y.pp#L1453

-- Modules
module :: { Module Name }
  : "module" module_name semi_binds "end"  { Module $2 $3 }

module_name :: { Name }
  : UPPERID  { $1 }

-- Types

-- "&" have the lowest precedence and is left associative.
-- Since we would like:  forall A. [A] -> Even  &  forall A. [A] -> Odd
-- to be parsed as:      (forall A. [A] -> Even) & (forall A. [A] -> Odd)
type :: { Type }
  : ftype "&" type            { And $1 $3 }
  | ftype                     { $1 }

ftype :: { Type }
  : atype "->" ftype          { Fun $1 $3 }
  | "forall" tvar "." ftype  { Forall $2 $4 }
  | atype                     { $1 }

atype :: { Type }
  : tvar                     { TVar $1 }
  | JAVACLASS                { JClass $1 }
  | "(" product_body ")"     { Product $2 }
  | "{" record_body "}"      { Record $2 }
  | "(" type ")"             { $2 }
  | "Unit"                   { Unit }

product_body :: { [Type] }
  : type "," type             { $1:[$3] }
  | type "," product_body     { $1:$3   }

record_body :: { [(Label, Type)] }
  : label ":" type                  { [($1, $3)]  }
  | label ":" type "," record_body  { ($1, $3):$5 }

label :: { Label }
  : LOWERID                  { $1 }

tvars :: { [Name] }
  : {- empty -}              { []    }
  | tvar tvars               { $1:$2 }

tvar :: { Name }
  : UPPERID                  { $1 }

-- Expressions

expr :: { Expr Name }
    : "/\\" tvar "." expr                 { BLam $2 $4  }
    | "\\" var_with_annot "." expr        { Lam $2 $4 }
    | "let" recflag and_binds "in" expr   { Let $2 $3 $5 }
    | "if" expr "then" expr "else" expr   { If $2 $4 $6 }
    | "-" INT %prec UMINUS                { Lit (Int (-$2)) }
    | infixexpr                           { $1 }

infixexpr :: { Expr Name }
    : infixexpr "*"  infixexpr  { PrimOp $1 (Arith J.Mult)   $3 }
    | infixexpr "/"  infixexpr  { PrimOp $1 (Arith J.Div)    $3 }
    | infixexpr "%"  infixexpr  { PrimOp $1 (Arith J.Rem)    $3 }
    | infixexpr "+"  infixexpr  { PrimOp $1 (Arith J.Add)    $3 }
    | infixexpr "-"  infixexpr  { PrimOp $1 (Arith J.Sub)    $3 }
    | infixexpr "<"  infixexpr  { PrimOp $1 (Compare J.LThan)  $3 }
    | infixexpr "<=" infixexpr  { PrimOp $1 (Compare J.LThanE) $3 }
    | infixexpr ">"  infixexpr  { PrimOp $1 (Compare J.GThan)  $3 }
    | infixexpr ">=" infixexpr  { PrimOp $1 (Compare J.GThanE) $3 }
    | infixexpr "==" infixexpr  { PrimOp $1 (Compare J.Equal)  $3 }
    | infixexpr "!=" infixexpr  { PrimOp $1 (Compare J.NotEq)  $3 }
    | infixexpr "&&" infixexpr  { PrimOp $1 (Logic J.CAnd)   $3 }
    | infixexpr "||" infixexpr  { PrimOp $1 (Logic J.COr)    $3 }
    | infixexpr ",," infixexpr  { Merge $1 $3 }
    | fexpr                     { $1 }

fexpr :: { Expr Name }
    : fexpr aexpr        { App  $1 $2 }
    | fexpr atype        { TApp $1 $2 }
    | aexpr              { $1 }

aexpr :: { Expr Name }
    : var                       { Var $1 }
    | lit                       { $1 }
    | "(" comma_exprs2 ")"      { Tuple $2 }
    | aexpr "." UNDERID         { Proj $1 $3 }
    | module_name "." var       { ModuleAccess $1 $3 }
    | javaexpr                  { $1 }
    | "{" semi_exprs "}"        { Seq $2 }
    | "{" recordlit_body "}"    { RecordLit $2 }
    | aexpr "with" "{" recordlit_body "}"  { RecordUpdate $1 $4 }
    | list_body                 { PrimList $1 }
    | "(" expr ")"              { $2 }

lit :: { Expr Name }
    : INT                       { Lit (Int $1)    }
    | STRING                    { Lit (String $1) }
    | BOOL                      { Lit (Bool $1)   }
    | CHAR                      { Lit (Char $1)   }
    | "()"                      { Lit UnitLit     }

javaexpr :: { Expr Name }
    : "new" JAVACLASS "(" comma_exprs0 ")"        { JNewObj $2 $4 }
    | JAVACLASS "." LOWERID "(" comma_exprs0 ")"  { JMethod (Static $1) $3 $5 undefined }
    | JAVACLASS "." LOWERID "()"                  { JMethod (Static $1) $3 [] undefined }
    | JAVACLASS "." field                         { JField  (Static $1) $3 undefined }
    | aexpr "." LOWERID "(" comma_exprs0 ")"      { JMethod (NonStatic $1) $3 $5 undefined }
    | aexpr "." LOWERID "()"                      { JMethod (NonStatic $1) $3 [] undefined }
    | aexpr "." field                             { JField  (NonStatic $1) $3 undefined }

recordlit_body :: { [(Label, Expr Name)] }
  : label "=" expr                     { [($1, $3)]  }
  | label "=" expr "," recordlit_body  { ($1, $3):$5 }

list_body :: { [Expr Name] }
    : "(" expr "::" list_body ")"  { $2:$4 }
    | "[" comma_exprs0 "]"         { $2 }

field :: { Name }
   : LOWERID { $1 }
   | UPPERID { $1 }

semi_exprs :: { [Expr Name] }
           : expr                { [$1] }
           | expr ";" semi_exprs { $1:$3 }

comma_exprs0 :: { [Expr Name] }
    : {- empty -}             { []    }
    | comma_exprs1            { $1    }

comma_exprs1 :: { [Expr Name] }
    : expr                    { [$1]  }
    | expr "," comma_exprs1   { $1:$3 }

comma_exprs2 :: { [Expr Name] }
    : expr "," expr           { [$1,$3]  }
    | expr "," comma_exprs2   { $1:$3     }

var_with_annot :: { (Name, Type) }
  : "(" var ":" type ")"         { ($2, $4) }
  | "(" var_with_annot ")"      { $2       }

bind :: { Bind Name }
    : var tvars var_annots_emp maybe_sig "=" expr
        { Bind { bindId       = $1
               , bindTargs    = $2
               , bindArgs     = $3
               , bindRhs      = $6
               , bindRhsAnnot = $4
               }
        }

maybe_sig :: { Maybe Type }
  : ":" type     { Just $2 }
  | {- empty -} { Nothing }

and_binds :: { [Bind Name] }
    : bind                      { [$1]  }
    | bind "and" and_binds      { $1:$3 }

semi_binds :: { [Bind Name] }
    : bind                      { [$1]  }
    | bind ";" semi_binds      { $1:$3 }

recflag :: { RecFlag }
  : "rec"       { Rec }
  | {- empty -} { NonRec }

var_annot :: { (Name, Type) }
    : "(" var ":" type ")"       { ($2, $4) }

var_annots_emp :: { [(Name, Type)] }
    : {- empty -}               { []    }
    | var_annot var_annots_emp  { $1:$2 }

var :: { Name }
    : LOWERID           { $1 }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

parseError :: [Token] -> P a
parseError tokens = PError ("Parse error before tokens:\n\t" ++ show tokens)

reader :: String -> Expr Name
reader src = case (parseExpr . lexer) src of
                 POk expr   -> expr
                 PError msg -> error msg
}
