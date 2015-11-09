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
import StringUtils

import JavaUtils
import Control.Monad.State
}

%name parseExpr expr
%tokentype { Located Token }
%monad     { Alex }
%lexer     { lexer } { L _ Teof }
%error     { parseError }

%token

  "("      { L _ Toparen }
  ")"      { L _ Tcparen }
  "["      { L _ Tobrack }
  "]"      { L _ Tcbrack }
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
  "type"   { L _ Ttype }
  "let"    { L _ Tlet }
  "rec"    { L _ Trec }
  "in"     { L _ Tin }
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
  "`"      { L _ Tbackquote }
  STRL     { L _ Tstrl }
  STRR     { L _ Tstrr }
  STREXPL  { L _ Tstrexpl }
  STREXPR  { L _ Tstrexpr }

  UPPER_IDENT  { L _ (Tupperid _) }
  LOWER_IDENT  { L _ (Tlowerid _) }
  UNDER_IDENT  { L _ (Tunderid $$) }
  SYMBOL_IDENT { L _ (Tsymbolid _) }

  JAVACLASS { L _ (Tjavaclass _) }
  "new"     { L _ Tnew }

  "module"  { L _ Tmodule }
  "import"  { L _ Timport }
  "package"  { L _ TPackage }

  INT      { L _ (Tint _) }
  SCHAR    { L _ (Tschar _) }
  BOOL     { L _ (Tbool _) }
  CHAR     { L _ (Tchar _) }
  "Empty"    { L _ Temptytree }
  "Fork"     { L _ Tnonemptytree }
  "()"     { L _ Tunitlit }
  "Unit"   { L _ Tunit }
  "error"  { L _ Terror}
  "L["     { L _ Tlistbegin}

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
%nonassoc NEG

%%

-- Note: There are times when it is more convenient to parse a more general
-- language than that which is actually intended, and check it later.

-- Parsers of different languages:
-- Haskell (GHC):  https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y.pp#L1453
-- Rust:           https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y

------------------------------------------------------------------------
-- Modules
------------------------------------------------------------------------

package :: { Maybe PackageName }
  : "package" packageIdent       { Just $2 }
  |                              { Nothing }

packageIdent :: { String }
  : ident    { unLoc $1 }
  | ident "." packageIdent { unLoc $1 ++ "." ++ $3 }

module :: { ReadModule }
  : "module" imports semi_binds  { Module (map unLoc $2) $3 `withLoc` $1 }

-- module_name :: { LReaderId }
--   : UPPER_IDENT  { toString $1 `withLoc` $1 }

imports :: { [ReadImport] }
  :                    { [] }
  | import ";" imports { $1:$3 }

import :: { ReadImport }
  : "import" packageIdent  { Import $2 `withLoc` $1 }


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type :: { ReadType }
  : "forall" ctyparams "." type  { foldr Forall (Forall (unLoc $ last $2) $4) (map unLoc $ init $2) }
  | monotype                     { $1 }

types :: { [ReadType] }
  : {- empty -}              { [] }
  | ftype types              { $1:$2 }

type_list :: { [ReadType] }
  : "[" comma_types1 "]"     { $2 }

comma_types1 :: { [ReadType] }
  : type                   { [$1]  }
  | type "," comma_types1  { $1:$3 }

comma_types2 :: { [ReadType] }
  : type "," comma_types1     { $1:$3   }

monotype :: { ReadType }
  : intertype "->" monotype  { Fun $1 $3 }
  | intertype                { $1 }

intertype :: { ReadType }
  : ftype "&" intertype      { And $1 $3 }
  | ftype                    { $1 }

ftype :: { ReadType }
  : ftype type_list  { foldl OpApp (OpApp $1 (head $2)) (tail $2) }
  | atype            { $1 }

atype :: { ReadType }
  : UPPER_IDENT              { TVar $ toString $1 }
  | JAVACLASS                { JClass $ toString $1 }
  | "Unit"                   { Unit }
  | "(" comma_types2 ")"     { TupleType $2 }
  | record_type              { $1 }
  | "(" type ")"             { $2 }

-- record types
record_type :: { ReadType }
  -- TODO: desugaring might be too early. But the benefit is avoid a traversal of the type structure later.
  : "{" record_type_fields_rev "}"      { foldl (\acc (l,t) -> And acc (RecordType [(l,t)])) (RecordType [(head (reverse $2))]) (tail (reverse $2)) }
  | "{" record_type_fields_rev "," "}"  { foldl (\acc (l,t) -> And acc (RecordType [(l,t)])) (RecordType [(head (reverse $2))]) (tail (reverse $2)) }

-- Allowing an extra "," before "}" will introduce a shift-reduce conflict;
-- but using left-recursion will solve it. :)
-- And Happy is more efficient at parsing left-recursive rules!
record_type_fields_rev :: { [(Label, ReadType)] }
  : record_type_field                             { [$1]  }
  | record_type_fields_rev "," record_type_field  { $3:$1 }

record_type_field :: { (Label, ReadType) }
  : label ":" type                                { ($1, $3) }

{-------------------------------------------------------------------------------
        Type parameters
-------------------------------------------------------------------------------}

typaram :: { Located ReadId }
  : UPPER_IDENT                    { toString $1 `withLoc` $1 }

typarams :: { [Located ReadId] }
  : {- empty -}                    { []    }
  | typaram typarams             { $1:$2 }

typarams1 :: { [Located ReadId] }
  : typaram typarams             { $1:$2 }

typaram_list :: { [Located ReadId] }
  : "[" comma_typarams1 "]"       { $2 }

typaram_list_or_empty :: { [Located ReadId] }
  : typaram_list                  { $1 }
  | {- empty -}                    { [] }

comma_typarams1 :: { [Located ReadId] }
  : typaram                       { [$1]  }
  | typaram "," comma_typarams1  { $1:$3 }

{-------------------------------------------------------------------------------
        constrainable type parameters
-------------------------------------------------------------------------------}

ctyparam :: { Located (ReadId, Maybe Type) }
  : UPPER_IDENT              { (toString $1, Nothing) `withLoc` $1 }
  | UPPER_IDENT "*" type     { (toString $1, Just $3) `withLoc` $1 }
  | "(" ctyparam ")"         { $2 }

comma_ctyparams1 :: { [Located (ReadId, Maybe Type)] }
  : ctyparam                            { [$1]  }
  | ctyparam "," comma_ctyparams1  { $1:$3 }

ctyparam_list :: { [Located (ReadId, Maybe Type)] }
  : "[" comma_ctyparams1 "]"       { $2 }

ctyparam_list_or_empty :: { [Located (ReadId, Maybe Type)] }
  : ctyparam_list                  { $1 }
  | {- empty -}                    { [] }


paren_ctyparam :: { Located (ReadId, Maybe Type) }
  : UPPER_IDENT                     { (toString $1, Nothing) `withLoc` $1 }
  | "(" UPPER_IDENT "*" type ")"    { (toString $2, Just $4) `withLoc` $1 }

ctyparams :: { [Located (ReadId, Maybe Type)] }
  : {- empty -}                     { []    }
  | paren_ctyparam ctyparams        { $1:$2 }

ctyparams1 :: { [Located (ReadId, Maybe Type)] }
  : paren_ctyparam ctyparams        { $1:$2 }

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: { ReadExpr }
    : "/\\" ctyparams1 "->" expr          { foldr (\t acc -> BLam (unLoc t) acc `withLoc` t) (BLam (unLoc $ last $2) $4 `withLoc` (last $2)) (init $2) }
    | "\\" params1 "->" expr             { foldr (\x acc -> Lam (unLoc x) acc `withLoc` x) (Lam (unLoc $ last $2) $4 `withLoc` (last $2)) (init $2) }
    | "let" recflag and_binds ";"  expr  { LetIn $2 $3 $5 `withLoc` $1 }
    | "let" recflag and_binds "in"  expr { LetIn $2 $3 $5 `withLoc` $1 }
    | "type" UPPER_IDENT typaram_list_or_empty "=" type ";"  expr  { Type (toString $2) (map unLoc $3) $5 $7 `withLoc` $1 }
    | "type" UPPER_IDENT typaram_list_or_empty "=" type expr       { Type (toString $2) (map unLoc $3) $5 $6 `withLoc` $1 }
    | "if" expr "then" expr "else" expr   { If $2 $4 $6 `withLoc` $1 }
    | "data" recflag and_databinds ";" expr        { Data $2 $3 $5 `withLoc` $1 }
    | "case" expr "of" alts               { Case $2 $4 `withLoc` $1 }
    | infixexpr0                          { $1 }
    | package module                      { EModule $1 (unLoc $2) (Lit UnitLit) `withLoc` $2 }
    | import ";" expr                     { EImport (unLoc $1) $3 `withLoc` $1 }
    | "-" fexpr %prec NEG                 { PrimOp (Lit (Int 0) `withLoc` $1) (Arith J.Sub) $2 `withLoc` $1 }

semi_exprs :: { [ReadExpr] }
           : expr                { [$1] }
           | expr ";" semi_exprs { $1:$3 }

comma_exprs0 :: { [ReadExpr] }
    : {- empty -}             { []    }
    | comma_exprs1            { $1    }

comma_exprs1 :: { [ReadExpr] }
    : expr                    { [$1]  }
    | expr "," comma_exprs1   { $1:$3 }

comma_exprs2 :: { [ReadExpr] }
    : expr "," expr           { [$1,$3]  }
    | expr "," comma_exprs2   { $1:$3    }

infixexpr0 :: { ReadExpr }
           : infixexpr                                  { $1 }
           | infixexpr0 infix_func infixexpr            { App (App $2 $1 `withLoc` $2) $3 `withLoc` $1 }

infix_func :: { ReadExpr }
           : SYMBOL_IDENT                   { Var (toString $1) `withLoc` $1 }
           | SYMBOL_IDENT type_list         { foldl (\acc t -> TApp acc t `withLoc` acc)
                                                    (TApp (Var (toString $1) `withLoc` $1) (head $2) `withLoc` $1)
                                                    (tail $2) }
           | "`" LOWER_IDENT "`"            { Var (toString $2) `withLoc` $2 }
           | "`" LOWER_IDENT type_list "`"  { foldl (\acc t -> TApp acc t `withLoc` acc)
                                                    (TApp (Var (toString $2) `withLoc` $2) (head $3) `withLoc` $2)
                                                    (tail $3) }

infixexpr :: { ReadExpr }
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

fexpr :: { ReadExpr }
    : fexpr aexpr      { App  $1 $2 `withLoc` $1 }
    | fexpr type_list  { foldl (\acc t -> TApp acc t `withLoc` acc) (TApp $1 (head $2) `withLoc` $1) (tail $2) }
    | aexpr            { $1 }
    | "error" "[" type "]" aexpr { Error $3 $5 `withLoc` $1}

aexpr :: { ReadExpr }
    : LOWER_IDENT               { Var (toString $1) `withLoc` $1 }
    | "(" SYMBOL_IDENT ")"      { Var (toString $2) `withLoc` $2 }
    | lit                       { $1 }
    | "(" comma_exprs2 ")"      { TupleCon $2 `withLoc` $1 }
    | aexpr "." UNDER_IDENT     { TupleProj $1 $3 `withLoc` $1 }
    -- | module_name "." ident     { ModuleAccess (unLoc $1) (unLoc $3) `withLoc` $1 }
    | javaexpr                  { $1 }
    | "{" semi_exprs "}"        { Seq $2 `withLoc` $1 }
    | record_construct                { $1 }
    | aexpr "with" "{" record_construct_fields_rev "}"  { RecordUpdate $1 (reverse $4) `withLoc` $1 }
    | constr_name               { ConstrIn (unLoc $1) `withLoc` $1 }
    | "(" expr ")"              { $2 }
    | "L[" comma_exprs1 "]"     { PolyList $2 `withLoc` $1}

javaexpr :: { ReadExpr }
    : "new" JAVACLASS "(" comma_exprs0 ")"        { JNew (toString $2) $4 `withLoc` $1 }
    | "Empty"                                       { JNew "f2j.FunctionalTree" [] `withLoc` $1 }
    | "Fork" "(" comma_exprs0 ")"                   { JNew "f2j.FunctionalTree" $3`withLoc` $1 }

    | JAVACLASS "." LOWER_IDENT "(" comma_exprs0 ")"  { JMethod (Static $ toString $1) (toString $3) $5 undefined `withLoc` $1 }
    | JAVACLASS "." LOWER_IDENT "()"                  { JMethod (Static $ toString $1) (toString $3) [] undefined `withLoc` $1 }
    | JAVACLASS "." LOWER_IDENT                       { JField  (Static $ toString $1) (toString $3) undefined `withLoc` $1 }
    | JAVACLASS "." UPPER_IDENT                       { JField  (Static $ toString $1) (toString $3) undefined `withLoc` $1 } -- Constants

    -- A dot can mean three things:
    -- (1) method invocation
    -- (2) field access
    -- (3) record elimination

    -- case length comma_exprs0
    -- when 0: method invocation
    --   (since the gap between the two parentheses distinguishes the string from the unit literal `()`)
    -- when 1: method invocation or application (with a parenthesized argument)
    -- else:   method invocation or application (with a tuple)
    | aexpr "." LOWER_IDENT "(" comma_exprs0 ")"  { Dot $1 (toString $3) (Just ($5, UnitImpossible)) `withLoc` $1 }

    -- method invocation or application
    | aexpr "." LOWER_IDENT "()"                  { Dot $1 (toString $3) (Just ([], UnitPossible)) `withLoc` $1 }

    -- field access or record elimination
    | aexpr "." LOWER_IDENT                       { Dot $1 (toString $3) Nothing `withLoc` $1 }

    -- Is this possible?
    -- | aexpr "." UPPER_IDENT                    { Dot $1 $3 Nothing `withLoc` $1 }

strlit :: { ReadExpr }
    : {- empty -}               { Lit (String []) `withLoc` (L (Loc 0 0) undefined) }
    | SCHAR strlit              { Lit (String (toSChar $1:unpackstr $2)) `withLoc` $1 }

interp :: { ReadExpr }
    : STREXPL expr STREXPR      { JMethod (Static "java.lang.String") "valueOf" [$2] undefined `withLoc` $1 }

interps :: { ReadExpr }
    : strlit                    { $1 }
    | strlit interp interps     { jconcat $1 (jconcat $2 $3) }

lit :: { ReadExpr }
    : INT                       { Lit (Int $ toInt $1)       `withLoc` $1 }
    | STRL interps STRR         { unLoc $2                   `withLoc` $1 }
    | BOOL                      { Lit (Bool $ toBool $1)     `withLoc` $1 }
    | CHAR                      { Lit (Char $ toChar $1)     `withLoc` $1 }
    | "()"                      { Lit UnitLit                `withLoc` $1 }

-- record-construction expr
record_construct :: { ReadExpr }
  : "{" record_construct_fields_rev "}"                     { RecordCon (reverse $2) `withLoc` $1 }
  | "{" record_construct_fields_rev "," "}"                 { RecordCon (reverse $2) `withLoc` $1 }

-- Allowing an extra "," before "}" will introduce a shift-reduce conflict;
-- but using left-recursion will solve it. :)
-- And Happy is more efficient at parsing left-recursive rules!
record_construct_fields_rev :: { [(Label, ReadExpr)] }
  : record_construct_field                                  { [$1]  }
  | record_construct_fields_rev "," record_construct_field  { $3:$1 }

record_construct_field :: { (Label, ReadExpr) }
  : label "=" expr                                          { ($1, $3) }

bind :: { ReadBind }
  : bind_lhs ctyparam_list_or_empty params maybe_ty_ascription "=" expr
  { Bind { bindId       = toString $1
         , bindTyParams = map unLoc $2
         , bindParams   = map unLoc $3
         , bindRhsTyAscription = $4
         , bindRhs      = $6
         }
  }

bind_lhs
    : LOWER_IDENT           { $1 }
    | "(" SYMBOL_IDENT ")"  { $2 }

and_binds :: { [ReadBind] }
    : bind                      { [$1]  }
    | bind "and" and_binds      { $1:$3 }

and_databinds :: { [DataBind]}
    : databind                       { [$1] }
    | databind "and" and_databinds   { $1:$3 }

databind :: { DataBind }
    : UPPER_IDENT typaram_list_or_empty "=" constrs_decl { DataBind (toString $1) (map unLoc $2) $4 }

semi_binds :: { [ReadModuleBind] }
    : modulebind                      { [$1]  }
    | modulebind ";" semi_binds       { $1:$3 }

modulebind :: { ReadModuleBind }
    : bind              { BindNonRec $1 }
    | "rec" and_binds   { BindRec $2 }

maybe_ty_ascription :: { Maybe ReadType }
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

constr_name :: { LReadId }
    : UPPER_IDENT  { toString $1 `withLoc` $1 }

alts :: { [Alt ReadId Type] }
    : alt               { [$1] }
    | alt "|" alts      { $1:$3 }

alt :: { Alt ReadId Type}
    : pattern "->" expr               { ConstrAlt $1 $3 }
    | "[" "]" "->" expr               { ConstrAlt ( PConstr (Constructor "empty" []) [] ) $4}
    | pat_var ":" pat_var "->" expr   { ConstrAlt ( PConstr (Constructor "cons" []) [$1,$3]) $5}

pattern1s :: { [Pattern] }
    : pattern1                 { [$1] }
    | pattern1 pattern1s       { $1:$2 }

pattern1 :: { Pattern }
    : constr_name              { PConstr (Constructor (unLoc $1) []) [] }
    | "(" pattern ")"          { $2 }
    | pat_var                  { $1 }

pattern :: { Pattern }
    : pattern1                 { $1 }
    | constr_name pattern1s    { PConstr (Constructor (unLoc $1) []) $2 }

pat_var :: { Pattern }
    : LOWER_IDENT              { PVar (toString $1) undefined }
    | "_"                      { PWildcard }

param :: { Located (ReadId, ReadType) }
  : LOWER_IDENT ":" type          { (toString $1, $3) `withLoc` $1 }

params :: { [Located (ReadId, ReadType)] }
  : {- empty -}                   { []    }
  | "(" param ")" params          { $2:$4 }

params1 :: { [Located (ReadId, ReadType)] }
  : "(" param ")" params          { $2:$4 }

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

ident :: { LReadId }
  : UPPER_IDENT  { toString $1 `withLoc` $1 }
  | LOWER_IDENT  { toString $1 `withLoc` $1 }

label :: { Label }
  : LOWER_IDENT  { toString $1 }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

instance Functor P where
  fmap  = liftM

instance Applicative P where
  pure  = return
  (<*>) = ap

parseError :: Located Token -> Alex a
parseError (L loc _) = alexError ("Parse error at " ++ show (line loc) ++ ":" ++ show (column loc))

reader :: String -> P ReadExpr
reader src = case (runAlex src parseExpr) of
               Left msg -> PError msg
               Right x  -> return x

-- Helper functions to extract located token value
toInt (L _ (Tint x)) = x
toBool (L _ (Tbool x)) = x
toChar (L _ (Tchar x)) = x
toSChar (L _ (Tschar x)) = x
toString (L _ tok) =
  case tok of
    Tjavaclass x -> x
    Tupperid x -> x
    Tlowerid x -> x
    Tsymbolid x -> x

-- Help functions of string interpolation
-- Returns a dummy list if not a string literal
unpackstr (L _ (Lit (String x))) = x
unpackstr _ = ['\0']

-- Concatenates two strings by Java method `concat()'
jconcat x y | null (unpackstr y) = x
            | null (unpackstr x) = y
            | otherwise = JMethod (NonStatic x) "concat" [y] undefined `withLoc` x
}
