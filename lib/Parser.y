{
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Parser
Description :  Parser for the source language.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Weixin Zhang <zhangweixinxd@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module Parser where

import qualified Language.Java.Syntax as J (Op (..))

import Src
import Lexer
import SrcLoc

import JavaUtils
}

%name parseExpr expr
%tokentype { Located Token }
%monad     { P }
%error     { parseError }

%token

  "("      { L _ Toparen }
  ")"      { L _ Tcparen }
  "["      { L _ Tobrack }
  "]"      { L _ Tcbrack }
  "::"     { L _ Tdcolon }
  "{"      { L _ Tocurly }
  "}"      { L _ Tccurly }
  "/\\"    { L _ Ttlam }
  "\\"     { L _ Tlam }
  ":"      { L _ Tcolon }
  ";"      { L _ Tsemi }
  "forall" { L _ Tforall }
  "->"     { L _ Tarrow }
  "."      { L _ Tdot }
  "&"      { L _ Tandtype }
  ",,"     { L _ Tmerge }
  "with"   { L _ Twith }
  "'"      { L _ Tquote }
  "type"   { L _ Ttype }
  "let"    { L _ Tlet }
  "rec"    { L _ Trec }
  "="      { L _ Teq }
  "and"    { L _ Tand }
  "if"     { L _ Tif }
  "then"   { L _ Tthen }
  "else"   { L _ Telse }
  ","      { L _ Tcomma }
  "data"   { L _ Tdata }
  "case"   { L _ Tcase }
  "|"      { L _ Tbar }
  "of"     { L _ Tof }
  "_"      { L _ Tunderscore }

  UPPER_IDENT  { L _ (Tupperid _) }
  LOWER_IDENT  { L _ (Tlowerid _) }
  UNDERID      { L _ (Tunderid $$) }

  JAVACLASS { L _ (Tjavaclass _) }
  "new"     { L _ Tnew }

  "module"  { L _ Tmodule }

  INT      { L _ (Tint _) }
  STRING   { L _ (Tstring _) }
  BOOL     { L _ (Tbool _) }
  CHAR     { L _ (Tchar _) }
  "Empty"    { L _ Temptytree }
  "Fork"     { L _ Tnonemptytree }
  "()"     { L _ Tunitlit }
  "Unit"   { L _ Tunit }
  "List"     { L _ Tlist }
  "head"     { L _ Tlisthead }
  "tail"     { L _ Tlisttail }
  "cons"     { L _ Tlistcons }
  "isNil"    { L _ Tlistisnil }
  "length"   { L _ Tlistlength }

  "*"      { L _ (Tprimop J.Mult)   }
  "/"      { L _ (Tprimop J.Div)    }
  "%"      { L _ (Tprimop J.Rem)    }
  "+"      { L _ (Tprimop J.Add)    }
  "-"      { L _ (Tprimop J.Sub)    }
  "<"      { L _ (Tprimop J.LThan)  }
  "<="     { L _ (Tprimop J.LThanE) }
  ">"      { L _ (Tprimop J.GThan)  }
  ">="     { L _ (Tprimop J.GThanE) }
  "=="     { L _ (Tprimop J.Equal)  }
  "!="     { L _ (Tprimop J.NotEq)  }
  "&&"     { L _ (Tprimop J.CAnd)   }
  "||"     { L _ (Tprimop J.COr)    }


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
  : "module" module_name "{" semi_binds "}"  { Module (unLoc $2) $4 `withLoc` $1 }

module_name :: { LReaderId }
  : UPPER_IDENT  { toUpperid $1 `withLoc` $1 }

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type :: { ReaderType }
  : "forall" ty_params "." type  { foldr Forall (Forall (unLoc $ last $2) $4) (map unLoc $ init $2) }
  | monotype                     { $1 }

types :: { [ReaderType] }
  : {- empty -}              { [] }
  | ftype types              { $1:$2 }

type_list :: { [ReaderType] }
  : "[" comma_types1 "]"     { $2 }

comma_types1 :: { [ReaderType] }
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
  : UPPER_IDENT              { TVar $ toUpperid $1 }
  | JAVACLASS                { JType (JClass $ toJavaclass $1) }
  | "Unit"                   { Unit }
  | "(" product_body ")"     { Product $2 }
  | record_type              { $1 }
  | "'" atype                { Thunk $2 }
  | "(" type ")"             { $2 }
  | "List" "[" type "]"      { ListOf $3}

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

ty_param :: { LReaderId }
  : UPPER_IDENT                    { toUpperid $1 `withLoc` $1 }

ty_params :: { [LReaderId] }
  : {- empty -}                    { []    }
  | ty_param ty_params             { $1:$2 }

ty_params1 :: { [LReaderId] }
  : ty_param ty_params             { $1:$2 }

ty_param_list :: { [LReaderId] }
  : "[" comma_ty_params1 "]"       { $2 }

ty_param_list_or_empty :: { [LReaderId] }
  : ty_param_list                  { $1 }
  | {- empty -}                    { [] }

comma_ty_params1 :: { [LReaderId] }
  : ty_param                       { [$1]  }
  | ty_param "," comma_ty_params1  { $1:$3 }

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: { ReaderExpr }
    : "/\\" ty_params1 "->" expr         { foldr (\t acc -> BLam (unLoc t) acc `withLoc` t) (BLam (unLoc $ last $2) $4 `withLoc` (last $2)) (init $2) }
    | "\\" params1 "->" expr             { foldr (\x acc -> Lam (unLoc x) acc `withLoc` x) (Lam (unLoc $ last $2) $4 `withLoc` (last $2)) (init $2) }
    | "let" recflag and_binds ";"  expr  { Let $2 $3 $5 `withLoc` $1 }
    | "type" UPPER_IDENT ty_param_list_or_empty "=" type ";"  expr  { Type (toUpperid $2) (map unLoc $3) $5 $7 `withLoc` $1 }
    | "type" UPPER_IDENT ty_param_list_or_empty "=" type expr       { Type (toUpperid $2) (map unLoc $3) $5 $6 `withLoc` $1 }
    | "if" expr "then" expr "else" expr   { If $2 $4 $6 `withLoc` $1 }
    | "-" INT %prec UMINUS                { Lit (Int $ negate $ toInt $2) `withLoc` $1 }
    | "data" UPPER_IDENT ty_param_list_or_empty "=" constrs_decl ";" expr { Data (toUpperid $2) (map unLoc $3) $5 $7 `withLoc` $1 }
    | "case" expr "of" patterns           { Case $2 $4 `withLoc` $1 }
    | infixexpr                           { $1 }
    | module expr                         { LetModule (unLoc $1) $2 `withLoc` $1 }

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
    : infixexpr "*"  infixexpr  { PrimOp $1 (Arith J.Mult)   $3 `withLoc` $1 }
    | infixexpr "/"  infixexpr  { PrimOp $1 (Arith J.Div)    $3 `withLoc` $1 }
    | infixexpr "%"  infixexpr  { PrimOp $1 (Arith J.Rem)    $3 `withLoc` $1 }
    | infixexpr "+"  infixexpr  { PrimOp $1 (Arith J.Add)    $3 `withLoc` $1 }
    | infixexpr "-"  infixexpr  { PrimOp $1 (Arith J.Sub)    $3 `withLoc` $1 }
    | infixexpr "<"  infixexpr  { PrimOp $1 (Compare J.LThan)  $3 `withLoc` $1 }
    | infixexpr "<=" infixexpr  { PrimOp $1 (Compare J.LThanE) $3 `withLoc` $1 }
    | infixexpr ">"  infixexpr  { PrimOp $1 (Compare J.GThan)  $3 `withLoc` $1 }
    | infixexpr ">=" infixexpr  { PrimOp $1 (Compare J.GThanE) $3 `withLoc` $1 }
    | infixexpr "==" infixexpr  { PrimOp $1 (Compare J.Equal)  $3 `withLoc` $1 }
    | infixexpr "!=" infixexpr  { PrimOp $1 (Compare J.NotEq)  $3 `withLoc` $1 }
    | infixexpr "&&" infixexpr  { PrimOp $1 (Logic J.CAnd)   $3 `withLoc` $1 }
    | infixexpr "||" infixexpr  { PrimOp $1 (Logic J.COr)    $3 `withLoc` $1 }
    | infixexpr ",," infixexpr  { Merge $1 $3 `withLoc` $1 }
    | fexpr                     { $1 }

fexpr :: { ReaderExpr }
    : fexpr aexpr      { App  $1 $2 `withLoc` $1 }
    | fexpr type_list  { foldl (\acc t -> TApp acc t `withLoc` acc) (TApp $1 (head $2) `withLoc` $1) (tail $2) }
    | aexpr            { $1 }

aexpr :: { ReaderExpr }
    : LOWER_IDENT               { Var (toLowerid $1) `withLoc` $1 }
    | lit                       { $1 }
    | "(" comma_exprs2 ")"      { Tuple $2 `withLoc` $1 }
    | aexpr "." UNDERID         { Proj $1 $3 `withLoc` $1 }
    | module_name "." ident     { ModuleAccess (unLoc $1) (unLoc $3) `withLoc` $1 }
    | javaexpr                  { $1 }
    | "{" semi_exprs "}"        { Seq $2 `withLoc` $1 }
    | record_construct                { $1 }
    | aexpr "with" "{" record_construct_fields_rev "}"  { RecordUpdate $1 (reverse $4) `withLoc` $1 }
    | "new" "List" "[" type "]" "()"  {PolyList [] $4 `withLoc` $1 }
    | "new" "List" "[" type "]" "(" comma_exprs0 ")"  { PolyList $7 $4 `withLoc` $1 }
    | "head" "(" fexpr ")"              { JProxyCall (JMethod (NonStatic $3 ) "head" [] undefined `withLoc` $1) undefined `withLoc` $1 }
    | "tail" "(" fexpr ")"              { JProxyCall (JMethod (NonStatic $3 ) "tail" [] undefined `withLoc` $1) undefined `withLoc` $1 }
    | "isNil" "(" fexpr ")"             { JMethod (NonStatic $3) "isEmpty" [] undefined `withLoc` $1 }
    | "length" "(" fexpr ")"            { JMethod (NonStatic $3) "length" [] undefined `withLoc` $1 }
    | "cons" "(" fexpr "," fexpr ")"    { JProxyCall (JNew "f2j.FunctionalList" [$3,$5] `withLoc` $1) undefined `withLoc` $1 }
    | constr_name               { ConstrTemp (unLoc $1) `withLoc` $1 }
    | "(" expr ")"              { $2 }

javaexpr :: { ReaderExpr }
    : "new" JAVACLASS "(" comma_exprs0 ")"        { JNew (toJavaclass $2) $4 `withLoc` $1 }
    | "Empty"                                       { JNew "f2j.FunctionalTree" [] `withLoc` $1 }
    | "Fork" "(" comma_exprs0 ")"                   { JNew "f2j.FunctionalTree" $3`withLoc` $1 }

    | JAVACLASS "." LOWER_IDENT "(" comma_exprs0 ")"  { JMethod (Static $ toJavaclass $1) (toLowerid $3) $5 undefined `withLoc` $1 }
    | JAVACLASS "." LOWER_IDENT "()"                  { JMethod (Static $ toJavaclass $1) (toLowerid $3) [] undefined `withLoc` $1 }
    | JAVACLASS "." LOWER_IDENT                       { JField  (Static $ toJavaclass $1) (toLowerid $3) undefined `withLoc` $1 }
    | JAVACLASS "." UPPER_IDENT                       { JField  (Static $ toJavaclass $1) (toLowerid $3) undefined `withLoc` $1 } -- Constants

    -- A dot can mean three things:
    -- (1) method invocation
    -- (2) field access
    -- (3) record elimination

    -- case length comma_exprs0
    -- when 0: method invocation
    --   (since the gap between the two parentheses distinguishes the string from the unit literal `()`)
    -- when 1: method invocation or application (with a parenthesized argument)
    -- else:   method invocation or application (with a tuple)
    | aexpr "." LOWER_IDENT "(" comma_exprs0 ")"  { Dot $1 (toLowerid $3) (Just ($5, UnitImpossible)) `withLoc` $1 }

    -- method invocation or application
    | aexpr "." LOWER_IDENT "()"                  { Dot $1 (toLowerid $3) (Just ([], UnitPossible)) `withLoc` $1 }

    -- field access or record elimination
    | aexpr "." LOWER_IDENT                       { Dot $1 (toLowerid $3) Nothing `withLoc` $1 }

    -- Is this possible?
    -- | aexpr "." UPPER_IDENT                    { Dot $1 $3 Nothing `withLoc` $1 }

lit :: { ReaderExpr }
    : INT                       { Lit (Int $ toInt $1)       `withLoc` $1 }
    | STRING                    { Lit (String $ toString $1) `withLoc` $1 }
    | BOOL                      { Lit (Bool $ toBool $1)     `withLoc` $1 }
    | CHAR                      { Lit (Char $ toChar $1)     `withLoc` $1 }
    | "()"                      { Lit UnitLit                `withLoc` $1 }

-- record-construction expr
record_construct :: { ReaderExpr }
  : "{" record_construct_fields_rev "}"                     { RecordCon (reverse $2) `withLoc` $1 }
  | "{" record_construct_fields_rev "," "}"                 { RecordCon (reverse $2) `withLoc` $1 }

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
  { Bind { bindId       = toLowerid $1
         , bindTyParams = map unLoc $2
         , bindParams   = map unLoc $3
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
    : constr_name types  { Constructor (unLoc $1) $2 }

constr_name :: { LReaderId }
    : UPPER_IDENT  { toUpperid $1 `withLoc` $1 }

patterns :: { [Alt ReaderId Type] }
    : pattern               { [$1] }
    | pattern "|" patterns  { $1:$3 }

pattern :: { Alt ReaderId Type}
    : constr_name pat_vars "->" expr  { ConstrAlt (Constructor (unLoc $1) []) $2 $4 }
    | "[" "]" "->" expr               { ConstrAlt (Constructor "empty" []) [] $4}
    | pat_var ":" pat_var "->" expr   { ConstrAlt (Constructor "cons" []) [$1,$3] $5}

pat_var :: { ReaderId }
  : LOWER_IDENT  { toLowerid $1 }
  | "_"          { "_" }

pat_vars :: { [ReaderId] }
  : {- empty -}       { [] }
  | pat_var pat_vars  { $1:$2 }

param :: { Located (ReaderId, ReaderType) }
  : LOWER_IDENT ":" type          { (toLowerid $1, $3) `withLoc` $1 }

params :: { [Located (ReaderId, ReaderType)] }
  : {- empty -}                   { []    }
  | "(" param ")" params          { $2:$4 }

params1 :: { [Located (ReaderId, ReaderType)] }
  : "(" param ")" params          { $2:$4 }

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

ident :: { LReaderId }
  : UPPER_IDENT  { toUpperid $1 `withLoc` $1 }
  | LOWER_IDENT  { toLowerid $1 `withLoc` $1 }

label :: { Label }
  : LOWER_IDENT  { toLowerid $1 }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

parseError :: [Located Token] -> P a
parseError []        = PError ("Parse error")
parseError (L loc _:_) = PError ("Parse error at " ++ show (line loc) ++ ":" ++ show (column loc))

reader :: String -> P ReaderExpr
reader src = parseExpr $ lexer src

-- Helper functions to extract located token value
toInt (L _ (Tint x)) = x
toString (L _ (Tstring x)) = x
toBool (L _ (Tbool x)) = x
toChar (L _ (Tchar x)) = x
toJavaclass (L _ (Tjavaclass x)) = x
toUpperid (L _ (Tupperid x)) = x
toLowerid (L _ (Tlowerid x)) = x
}
