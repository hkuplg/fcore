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

  INTEGER  { Tinteger $$ }
  STRING   { Tstring $$ }
  BOOLEAN  { Tboolean $$ }
  CHAR     { Tchar $$ }

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

-- There are times when it is more convenient to parse a more general
-- language than that which is actually intended, and check it later.

-- Reference for rules:
-- https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y.pp#L1453

expr :: { Expr Name }
     : infixexpr %prec EOF      { $1 }

infixexpr :: { Expr Name }
    : expr10                    { $1 }
    | infixexpr "*"  infixexpr  { PrimOp $1 (Arith J.Mult)   $3 }
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

expr10 :: { Expr Name }
    : "/\\" tvar "." expr                 { BLam $2 $4  }
    | "\\" var_with_annot "." expr        { Lam $2 $4 }
    | "let" recflag and_binds "in" expr   { Let $2 $3 $5 }
    | "if" expr "then" expr "else" expr   { If $2 $4 $6 }
    | "-" INTEGER %prec UMINUS            { Lit (Integer (-$2)) }
    | fexp                                { $1 }

fexp :: { Expr Name }
    : fexp aexp         { App  $1 $2 }
    | fexp typ          { TApp $1 $2 }
    | aexp              { $1 }

aexp :: { Expr Name }
    : aexp1             { $1 }

aexp1 :: { Expr Name }
    : var                       { Var $1 }
    | INTEGER                   { Lit (Integer $1) }
    | STRING                    { Lit (String $1) }
    | BOOLEAN                   { Lit (Boolean $1) }
    | CHAR                      { Lit (Char $1) }
    | aexp "." UNDERID          { Proj $1 $3 }
    | "(" comma_exprs ")"       { Tuple $2 }
    | "(" expr ")"              { $2 }
    -- Java
    | JAVACLASS "." LOWERID "(" comma_exprs_emp ")" { JMethod (Left $1) $3 $5 "" }
    | aexp "." LOWERID "(" comma_exprs_emp ")"      { JMethod (Right $1) $3 $5 "" }
    | JAVACLASS "." id { JField (Left $1) $3 "" }
    | aexp "." id      { JField (Right $1) $3 "" }
    | "new" JAVACLASS "(" comma_exprs_emp ")"       { JNewObj $2 $4 }
    -- Sequence of exprs
    | "{" semi_exprs "}"        { Seq $2 }
    -- primitive list
    | listexp                   { $1 }

-- listexp "." LOWERID       { JMethod (Left (PrimList $1))) $3 [] ""}

listexp :: { Expr String }
    : listexps                  { PrimList $1 }

listexps :: { [Expr String] }
    : "(" expr "::" listexps ")"  { $2:$4 }
    | "[" comma_exprs_emp "]" { $2 }

id :: { Name }
   : LOWERID { $1 }
   | UPPERID { $1 }

semi_exprs :: { [Expr Name] }
           : expr                { [$1] }
           | expr ";" semi_exprs { $1:$3 }

comma_exprs :: { [Expr Name] }
    : expr "," expr             { [$1, $3] }
    | expr "," comma_exprs      { $1:$3    }

comma_exprs_emp :: { [Expr Name] }
    : {- empty -}   { []   }
    | expr          { [$1] }
    | comma_exprs   { $1   }

var_with_annot :: { (Name, Type) }
  : "(" var ":" typ ")"         { ($2, $4) }
  | "(" var_with_annot ")"      { $2       }

bind :: { Bind Name }
    : var tvars_emp var_annots_emp maybe_sig "=" expr
        { Bind { bindId       = $1
               , bindTargs    = $2
               , bindArgs     = $3
               , bindRhs      = $6
               , bindRhsAnnot = $4
               }
        }

maybe_sig :: { Maybe Type }
  : ":" typ     { Just $2 }
  | {- empty -} { Nothing }

and_binds :: { [Bind Name] }
    : bind                      { [$1]  }
    | bind "and" and_binds      { $1:$3 }

recflag :: { RecFlag }
  : "rec"       { Rec }
  | {- empty -} { NonRec }

typ :: { Type }
    : "forall" tvar "." typ       { Forall $2 $4 }

    -- Require an atyp on the LHS so that `for A. A -> A` cannot be parsed
    -- as `(for A. A) -> A`, since `for A. A` is not a valid atyp.
    | atyp "->" typ     { Fun $1 $3 }
    | atyp              { $1 }
    | "[" atyp "]"     { ListOf $2 }
    --| typ "&" atyp      { And $1 $3 }


atyp :: { Type }
    : tvar                      { TyVar $1 }
    | "(" typ ")"               { $2 }
    | "(" comma_typs ")"        { Product $2 }
    | JAVACLASS                 { JClass $1 }

comma_typs :: { [Type] }
    : typ "," typ               { $1:[$3] }
    | typ "," comma_typs        { $1:$3   }

tvars_emp :: { [Name] }
  : {- empty -}         { []    }
  | tvar tvars_emp      { $1:$2 }

var_annot :: { (Name, Type) }
    : "(" var ":" typ ")"       { ($2, $4) }

var_annots_emp :: { [(Name, Type)] }
    : {- empty -}               { []    }
    | var_annot var_annots_emp  { $1:$2 }

var :: { Name }
    : LOWERID           { $1 }

tvar :: { Name }
    : UPPERID           { $1 }

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
