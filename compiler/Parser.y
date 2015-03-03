{
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Parser
Description :  Parser for the source language.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module Parser where

import qualified Language.Java.Syntax as J (Op (..))

import Src
import Lexer

import JavaUtils
}

%name parseExpr expr
%tokentype { Located Token }
%monad     { P }
%error     { parseError }

%token

  "("      { Located _ Toparen }
  ")"      { Located _ Tcparen }
  "["      { Located _ Tobrack }
  "]"      { Located _ Tcbrack }
  "::"     { Located _ Tdcolon }
  "{"      { Located _ Tocurly }
  "}"      { Located _ Tccurly }
  "/\\"    { Located _ Ttlam }
  "\\"     { Located _ Tlam }
  ":"      { Located _ Tcolon }
  ";"      { Located _ Tsemi }
  "forall" { Located _ Tforall }
  "->"     { Located _ Tarrow }
  "."      { Located _ Tdot }
  "&"      { Located _ Tandtype }
  ",,"     { Located _ Tmerge }
  "with"   { Located _ Twith }
  "'"      { Located _ Tquote }
  "type"   { Located _ Ttype }
  "let"    { Located _ Tlet }
  "rec"    { Located _ Trec }
  "="      { Located _ Teq }
  "and"    { Located _ Tand }
  "if"     { Located _ Tif }
  "then"   { Located _ Tthen }
  "else"   { Located _ Telse }
  ","      { Located _ Tcomma }

  "data"   { Located _ Tdata }
  "case"   { Located _ Tcase }
  "|"      { Located _ Tbar }
  "of"     { Located _ Tof }

  UPPER_IDENT  { Located _ (Tupperid $$) }
  LOWER_IDENT  { Located _ (Tlowerid $$) }
  UNDERID      { Located _ (Tunderid $$) }

  JAVACLASS { Located _ (Tjavaclass $$) }
  "new"     { Located _ Tnew }

  "module"  { Located _ Tmodule }

  INT      { Located _ (Tint $$) }
  STRING   { Located _ (Tstring $$) }
  BOOL     { Located _ (Tbool $$) }
  "Empty"    { Located _ Temptytree }
  "Fork"     { Located _ Tnonemptytree}
  CHAR     { Located _ (Tchar $$) }
  "()"     { Located _ Tunitlit }
  "Unit"   { Located _ Tunit }
  "List"     { Located _ Tlist }
  "head"     { Located _ Tlisthead }
  "tail"     { Located _ Tlisttail }
  "cons"     { Located _ Tlistcons }
  "isNil"    { Located _ Tlistisnil }
  "length"   { Located _ Tlistlength }

  "*"      { Located _ (Tprimop J.Mult)   }
  "/"      { Located _ (Tprimop J.Div)    }
  "%"      { Located _ (Tprimop J.Rem)    }
  "+"      { Located _ (Tprimop J.Add)    }
  "-"      { Located _ (Tprimop J.Sub)    }
  "<"      { Located _ (Tprimop J.LThan)  }
  "<="     { Located _ (Tprimop J.LThanE) }
  ">"      { Located _ (Tprimop J.GThan)  }
  ">="     { Located _ (Tprimop J.GThanE) }
  "=="     { Located _ (Tprimop J.Equal)  }
  "!="     { Located _ (Tprimop J.NotEq)  }
  "&&"     { Located _ (Tprimop J.CAnd)   }
  "||"     { Located _ (Tprimop J.COr)    }


-- Precedence and associativity directives
%nonassoc EOF

%right ";"
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

-- Parsers of different languages:
-- Haskell (GHC):  https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y.pp#L1453
-- Rust:           https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y

------------------------------------------------------------------------
-- Modules
------------------------------------------------------------------------

module :: { ReaderModule }
  : "module" module_name "{" semi_binds "}"  { Module $2 $4 }

module_name :: { ReaderId }
  : UPPER_IDENT  { $1 }

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type :: { ReaderType }
  : "forall" ty_params "." type  { foldr Forall (Forall (last $2) $4) (init $2) }
  | monotype                     { $1 }

types :: { [Type] }
  : {- empty -}              { [] }
  | atype types              { $1:$2 }

type_list :: { [Type] }
  : "[" comma_types1 "]"     { $2 }

comma_types1 :: { [Type] }
  : type                   { [$1]  }
  | type "," comma_types1  { $1:$3 }

monotype :: { ReaderType }
  : intertype "->" monotype  { Fun $1 $3 }
  | intertype                { $1 }

intertype :: { ReaderType }
  : ftype "&" intertype      { And $1 $3 }
  | ftype                    { $1 }

ftype :: { ReaderType }
  : ftype type_list  { foldl OpApp (OpApp $1 (head $2)) (tail $2) }
  | atype            { $1 }

atype :: { ReaderType }
  : UPPER_IDENT              { TVar $1 }
  | JAVACLASS                { JType (JClass $1) }
  | "Unit"                   { Unit }
  | "(" product_body ")"     { Product $2 }
  | record_type              { $1 }
  | "'" atype                { Thunk $2 }
  | "(" type ")"             { $2 }
  | "List" "<" type ">"      { ListOf $3}

product_body :: { [ReaderType] }
  : type "," type             { $1:[$3] }
  | type "," product_body     { $1:$3   }

-- record types
record_type :: { ReaderType }
  -- TODO: desugaring might be too early. But the benefit is avoid a traversal of the type structure later.
  : "{" record_type_fields_rev "}"      { foldl (\acc (l,t) -> And acc (RecordType [(l,t)])) (RecordType [(head (reverse $2))]) (tail (reverse $2)) }
  | "{" record_type_fields_rev "," "}"  { foldl (\acc (l,t) -> And acc (RecordType [(l,t)])) (RecordType [(head (reverse $2))]) (tail (reverse $2)) }

-- Allowing an extra "," before "}" will introduce a shift-reduce conflict;
-- but using left-recursion will solve it. :)
-- And Happy is more efficient at parsing left-recursive rules!
record_type_fields_rev :: { [(Label, ReaderType)] }
  : record_type_field                             { [$1]  }
  | record_type_fields_rev "," record_type_field  { $3:$1 }

record_type_field :: { (Label, ReaderType) }
  : label ":" type                                { ($1, $3) }

ty_param :: { ReaderId }
  : UPPER_IDENT                    { $1 }

ty_params :: { [ReaderId] }
  : {- empty -}                    { []    }
  | ty_param ty_params             { $1:$2 }

ty_params1 :: { [ReaderId] }
  : ty_param ty_params             { $1:$2 }

ty_param_list :: { [ReaderId] }
  : "[" comma_ty_params1 "]"       { $2 }

ty_param_list_or_empty :: { [ReaderId] }
  : ty_param_list                  { $1 }
  | {- empty -}                    { [] }

comma_ty_params1 :: { [ReaderId] }
  : ty_param                       { [$1]  }
  | ty_param "," comma_ty_params1  { $1:$3 }

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: { ReaderExpr }
    : "/\\" ty_params1 "->" expr         { foldr BLam (BLam (last $2) $4) (init $2) }
    | "\\" params1 "->" expr             { foldr Lam (Lam (last $2) $4) (init $2) }
    | "let" recflag and_binds ";"  expr  { Let $2 $3 $5 }
    | "type" UPPER_IDENT ty_param_list_or_empty "=" type ";"  expr  { Type $2 $3 $5 $7 }
    | "type" UPPER_IDENT ty_param_list_or_empty "=" type expr       { Type $2 $3 $5 $6 }
    | "if" expr "then" expr "else" expr   { If $2 $4 $6 }
    | "-" INT %prec UMINUS                { Lit (Int (-$2)) }
    | "data" UPPER_IDENT ty_params "=" constrs_decl ";" expr { Data $2 $3 $5 $7 }
    | "case" expr "of" patterns           { Case $2 $4 }
    | infixexpr                           { $1 }
    | module expr                         { LetModule $1 $2 }

semi_exprs :: { [ReaderExpr] }
           : expr                { [$1] }
           | expr ";" semi_exprs { $1:$3 }

comma_exprs0 :: { [ReaderExpr] }
    : {- empty -}             { []    }
    | comma_exprs1            { $1    }

comma_exprs1 :: { [ReaderExpr] }
    : expr                    { [$1]  }
    | expr "," comma_exprs1   { $1:$3 }

comma_exprs2 :: { [ReaderExpr] }
    : expr "," expr           { [$1,$3]  }
    | expr "," comma_exprs2   { $1:$3    }

infixexpr :: { ReaderExpr }
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

fexpr :: { ReaderExpr }
    : fexpr aexpr      { App  $1 $2 }
    | fexpr type_list  { foldl TApp (TApp $1 (head $2)) (tail $2) }
    | aexpr            { $1 }

aexpr :: { ReaderExpr }
    : LOWER_IDENT               { Var $1 }
    | lit                       { $1 }
    | "(" comma_exprs2 ")"      { Tuple $2 }
    | aexpr "." UNDERID         { Proj $1 $3 }
    | module_name "." ident     { ModuleAccess $1 $3 }
    | javaexpr                  { $1 }
    | "{" semi_exprs "}"        { Seq $2 }
    | record_construct                { $1 }
    | aexpr "with" "{" record_construct_fields_rev "}"  { RecordUpdate $1 (reverse $4) }
    | "new" "List" "<" type ">" "()"  {PolyList [] $4}
    | "new" "List" "<" type ">" "(" comma_exprs0 ")"  {PolyList $7 $4}
    | "head" "(" fexpr ")"              { JProxyCall (JMethod (NonStatic $3 ) "head" [] undefined) undefined}
    | "tail" "(" fexpr ")"              { JProxyCall (JMethod (NonStatic $3 ) "tail" [] undefined) undefined}
    | "isNil" "(" fexpr ")"             { JMethod (NonStatic $3) "isEmpty" [] undefined}
    | "length" "(" fexpr ")"            { JMethod (NonStatic $3) "length" [] undefined}
    | "cons" "(" fexpr "," fexpr ")"    { JProxyCall (JNew "f2j.FunctionalList" [$3,$5]) undefined}
    | constr_name               { ConstrTemp $1 }
    | "(" expr ")"              { $2 }

aexprs :: { [ReaderExpr] }
    :  {- empty -}            { [] }
    | aexpr aexprs            { $1:$2 }

javaexpr :: { ReaderExpr }
    : "new" JAVACLASS "(" comma_exprs0 ")"        { JNew $2 $4 }
    | "Empty"                                       { JNew "f2j.FunctionalTree" [] }
    | "Fork" "(" comma_exprs0 ")"                   { JNew "f2j.FunctionalTree" $3}

    | JAVACLASS "." LOWER_IDENT "(" comma_exprs0 ")"  { JMethod (Static $1) $3 $5 undefined }
    | JAVACLASS "." LOWER_IDENT "()"                  { JMethod (Static $1) $3 [] undefined }
    | JAVACLASS "." LOWER_IDENT                       { JField  (Static $1) $3 undefined }
    | JAVACLASS "." UPPER_IDENT                       { JField  (Static $1) $3 undefined } -- Constants

    -- A dot can mean three things:
    -- (1) method invocation
    -- (2) field access
    -- (3) record elimination

    -- case length comma_exprs0
    -- when 0: method invocation
    --   (since the gap between the two parentheses distinguishes the string from the unit literal `()`)
    -- when 1: method invocation or application (with a parenthesized argument)
    -- else:   method invocation or application (with a tuple)
    | aexpr "." LOWER_IDENT "(" comma_exprs0 ")"  { Dot $1 $3 (Just ($5, UnitImpossible)) }

    -- method invocation or application
    | aexpr "." LOWER_IDENT "()"                  { Dot $1 $3 (Just ([], UnitPossible)) }

    -- field access or record elimination
    | aexpr "." LOWER_IDENT                       { Dot $1 $3 Nothing }

    -- Is this possible?
    -- | aexpr "." UPPER_IDENT                    { Dot $1 $3 Nothing }

lit :: { ReaderExpr }
    : INT                       { Lit (Int $1)    }
    | STRING                    { Lit (String $1) }
    | BOOL                      { Lit (Bool $1)   }
    | CHAR                      { Lit (Char $1)   }
    | "()"                      { Lit UnitLit     }

-- record-construction expr
record_construct :: { ReaderExpr }
  : "{" record_construct_fields_rev "}"                     { RecordCon (reverse $2) }
  | "{" record_construct_fields_rev "," "}"                 { RecordCon (reverse $2) }

-- Allowing an extra "," before "}" will introduce a shift-reduce conflict;
-- but using left-recursion will solve it. :)
-- And Happy is more efficient at parsing left-recursive rules!
record_construct_fields_rev :: { [(Label, ReaderExpr)] }
  : record_construct_field                                  { [$1]  }
  | record_construct_fields_rev "," record_construct_field  { $3:$1 }

record_construct_field :: { (Label, ReaderExpr) }
  : label "=" expr                                          { ($1, $3) }

bind :: { ReaderBind }
  : LOWER_IDENT ty_param_list_or_empty params maybe_ty_ascription "=" expr
  { Bind { bindId       = $1
         , bindTyParams = $2
         , bindParams   = $3
         , bindRhsTyAscription = $4
         , bindRhs      = $6
         }
  }

and_binds :: { [ReaderBind] }
    : bind                      { [$1]  }
    | bind "and" and_binds      { $1:$3 }

semi_binds :: { [ReaderBind] }
    : bind                      { [$1]  }
    | bind ";" and_binds      { $1:$3 }

maybe_ty_ascription :: { Maybe ReaderType }
  : ":" type     { Just $2 }
  | {- empty -} { Nothing }

recflag :: { RecFlag }
  : "rec"       { Rec }
  | {- empty -} { NonRec }

constrs_decl :: { [Constructor] }
    : constr_decl { [$1] }
    | constr_decl "|" constrs_decl  { $1:$3 }

constr_decl :: { Constructor }
    : constr_name types  { Constructor $1 $2 }

constr_name :: { ReaderId }
  : UPPER_IDENT  { $1 }

patterns :: { [Alt ReaderId Type] }
    : pattern               { [$1] }
    | pattern "|" patterns  { $1:$3 }

pattern :: { Alt ReaderId Type}
    : constr_name pat_vars "->" expr  { ConstrAlt (Constructor $1 []) $2 $4 }

pat_var :: { ReaderId }
  : LOWER_IDENT  { $1 }

pat_vars :: { [ReaderId] }
  : {- empty -}       { [] }
  | pat_var pat_vars  { $1:$2 }

param :: { (ReaderId, ReaderType) }
  : LOWER_IDENT ":" type          { ($1, $3) }

params :: { [(ReaderId, ReaderType)] }
  : {- empty -}                   { []    }
  | "(" param ")" params          { $2:$4 }

params1 :: { [(ReaderId, ReaderType)] }
  : "(" param ")" params          { $2:$4 }

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

ident :: { ReaderId }
  : UPPER_IDENT  { $1 }
  | LOWER_IDENT  { $1 }

label :: { Label }
  : LOWER_IDENT  { $1 }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

parseError :: [Located Token] -> P a
parseError []        = PError ("Parse error")
parseError (token:_) = PError ("Parse error at " ++ show line ++ ":" ++ show col)
  where (line, col) = getLocation token

reader :: String -> ReaderExpr
reader src = case (parseExpr . lexer) src of
                 POk expr   -> expr
                 PError msg -> error msg
}
