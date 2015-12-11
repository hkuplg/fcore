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

%name happyParseProgram program
%name happyParseExpr    expr

%tokentype { Located Token }
%monad     { Alex }
%lexer     { lexer } { L _ Teof }
%error     { parseError }

%token

  -- Keywords

  "Unit"   { L _ Tunit }
  "and"    { L _ Tand }
  "case"   { L _ Tcase }
  "data"   { L _ Tdata }
  "def"    { L _ Tdef }
  "else"   { L _ Telse }
  "error"  { L _ Terror}
  "forall" { L _ Tforall }
  "if"     { L _ Tif }
  "import" { L _ Timport }
  "in"     { L _ Tin }
  "let"    { L _ Tlet }
  "new"    { L _ Tnew }
  "of"     { L _ Tof }
  "rec"    { L _ Trec }
  "then"   { L _ Tthen }
  "type"   { L _ Ttype }
  "with"   { L _ Twith }
  BOOL     { L _ (Tbool _) }

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
  "->"     { L _ Tarrow }
  "."      { L _ Tdot }
  "&"      { L _ Tandtype }
  ",,"     { L _ Tmerge }
  "="      { L _ Teq }
  ","      { L _ Tcomma }
  "|"      { L _ Tbar }
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

  INT      { L _ (Tint _) }
  SCHAR    { L _ (Tschar _) }
  CHAR     { L _ (Tchar _) }
  "()"     { L _ Tunitlit }
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

{-
*Note:* There are times when it is more convenient to parse a more general
language than that which is actually intended, and check it later.

Parsers of other languages:
* Haskell (GHC):  https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y
* Rust:           https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y
-}

program :: { Program }
  : imports declarations { Program { pgmImports = $1, pgmDecls = $2 } }

  -- Backward compatibility for the expression language before:
  -- Wrap the whole expression in `def main() = { ... }`
  | imports expr         { Program { pgmImports = $1, pgmDecls = [liftExprToDecl "main" $2] } }

imports :: { [Import] }
  : {- empty -}    { [] }
  | import imports { $1:$2 }

import :: { Import }
  : "import" module_path { Import { importModulePath = $2 } }

declarations :: { [Declaration] }
  : {- empty -}              { [] }
  | declaration declarations { $1:$2 }

declaration :: { Declaration }
  : "def" bind { DefDecl $2 }

------------------------------------------------------------------------
-- Modules
------------------------------------------------------------------------

module_path :: { Located ModulePath }
  : ident                 { $1 }
  | ident "." module_path { (unLoc $1 ++ "." ++ unLoc $3) `withLoc` $1 }

module :: { ReadModule }
  : poormens_imports semi_binds  { Module (map unLoc $1) $2 }

poormens_imports :: { [ReadImport] }
  :                { [] }
  | poormens_import poormens_imports { $1:$2 }

poormens_import :: { ReadImport }
  : "import" module_path  { PoorMensImport (unLoc $2) `withLoc` $1 }


------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type :: { Type }
  : "forall" ctyparams "." type  { foldr Forall (Forall (unLoc $ last $2) $4) (map unLoc $ init $2) }
  | monotype                     { $1 }

types :: { [Type] }
  : {- empty -}              { [] }
  | ftype types              { $1:$2 }

type_list :: { [Type] }
  : "[" comma_types1 "]"     { $2 }

comma_types1 :: { [Type] }
  : type                   { [$1]  }
  | type "," comma_types1  { $1:$3 }

comma_types2 :: { [Type] }
  : type "," comma_types1     { $1:$3   }

monotype :: { Type }
  : intertype "->" monotype  { Fun $1 $3 }
  | intertype                { $1 }

intertype :: { Type }
  : ftype "&" intertype      { And $1 $3 }
  | ftype                    { $1 }

ftype :: { Type }
  : ftype type_list  { foldl OpApp (OpApp $1 (head $2)) (tail $2) }
  | atype            { $1 }

atype :: { Type }
  : UPPER_IDENT              { TVar $ toString $1 }
  | JAVACLASS                { JClass $ toString $1 }
  | "Unit"                   { Unit }
  | "(" comma_types2 ")"     { TupleType $2 }
  | record_type              { $1 }
  | "(" type ")"             { $2 }

-- record types
record_type :: { Type }
  -- TODO: desugaring might be too early. But the benefit is avoid a traversal of the type structure later.
  : "{" record_type_fields_rev "}"      { foldl (\acc (l,t) -> And acc (RecordType [(l,t)])) (RecordType [(head (reverse $2))]) (tail (reverse $2)) }
  | "{" record_type_fields_rev "," "}"  { foldl (\acc (l,t) -> And acc (RecordType [(l,t)])) (RecordType [(head (reverse $2))]) (tail (reverse $2)) }

-- Allowing an extra "," before "}" will introduce a shift-reduce conflict;
-- but using left-recursion will solve it. :)
-- And Happy is more efficient at parsing left-recursive rules!
record_type_fields_rev :: { [(Label, Type)] }
  : record_type_field                             { [$1]  }
  | record_type_fields_rev "," record_type_field  { $3:$1 }

record_type_field :: { (Label, Type) }
  : label ":" type                                { ($1, $3) }

{-------------------------------------------------------------------------------
        Type parameters
-------------------------------------------------------------------------------}

typaram :: { Located Name }
  : UPPER_IDENT                    { toString $1 `withLoc` $1 }

typarams :: { [Located Name] }
  : {- empty -}                    { []    }
  | typaram typarams             { $1:$2 }

typarams1 :: { [Located Name] }
  : typaram typarams             { $1:$2 }

typaram_list :: { [Located Name] }
  : "[" comma_typarams1 "]"       { $2 }

typaram_list_or_empty :: { [Located Name] }
  : typaram_list                  { $1 }
  | {- empty -}                    { [] }

comma_typarams1 :: { [Located Name] }
  : typaram                       { [$1]  }
  | typaram "," comma_typarams1  { $1:$3 }

{-------------------------------------------------------------------------------
        constrainable type parameters
-------------------------------------------------------------------------------}

ctyparam :: { Located (Name, Maybe Type) }
  : UPPER_IDENT              { (toString $1, Nothing) `withLoc` $1 }
  | UPPER_IDENT "*" type     { (toString $1, Just $3) `withLoc` $1 }
  | "(" ctyparam ")"         { $2 }

comma_ctyparams1 :: { [Located (Name, Maybe Type)] }
  : ctyparam                            { [$1]  }
  | ctyparam "," comma_ctyparams1  { $1:$3 }

ctyparam_list :: { [Located (Name, Maybe Type)] }
  : "[" comma_ctyparams1 "]"       { $2 }

ctyparam_list_or_empty :: { [Located (Name, Maybe Type)] }
  : ctyparam_list                  { $1 }
  | {- empty -}                    { [] }


paren_ctyparam :: { Located (Name, Maybe Type) }
  : UPPER_IDENT                     { (toString $1, Nothing) `withLoc` $1 }
  | "(" UPPER_IDENT "*" type ")"    { (toString $2, Just $4) `withLoc` $1 }

ctyparams :: { [Located (Name, Maybe Type)] }
  : {- empty -}                     { []    }
  | paren_ctyparam ctyparams        { $1:$2 }

ctyparams1 :: { [Located (Name, Maybe Type)] }
  : paren_ctyparam ctyparams        { $1:$2 }

------------------------------------------------------------------------
-- Expressions
------------------------------------------------------------------------

expr :: { ReadExpr }
    : "/\\" ctyparams1 "->" expr  { foldr (\t acc -> BLam (unLoc t) acc `withLoc` t) (BLam (unLoc $ last $2) $4 `withLoc` (last $2)) (init $2) }
    | "\\"  param_list1 "->" expr { foldr (\x acc -> Lam x acc `withLoc` (fst x)) (Lam (last $2) $4 `withLoc` (fst (last $2))) (init $2) }
    | "let" recflag and_binds "in"  expr { LetIn $2 $3 $5 `withLoc` $1 }
    | "type" UPPER_IDENT typaram_list_or_empty "=" type "in"  expr  { Type (toString $2) (map unLoc $3) $5 $7 `withLoc` $1 }
    | "if" expr "then" expr "else" expr   { If $2 $4 $6 `withLoc` $1 }
    | "data" recflag and_databinds "in" expr        { Data $2 $3 $5 `withLoc` $1 }
    | "case" expr "of" alts               { Case $2 $4 `withLoc` $1 }
    | infixexpr0                          { $1 }
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
  : bind_lhs ctyparam_list_or_empty param_list maybe_ty_ascription "=" expr
  { Bind { bindName       = toString $1
         , bindTyParams = map unLoc $2
         , bindParams   = $3
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

maybe_ty_ascription :: { Maybe Type }
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

constr_name :: { Located Name }
    : UPPER_IDENT  { toString $1 `withLoc` $1 }

alts :: { [Alt Name Type] }
    : alt               { [$1] }
    | alt "|" alts      { $1:$3 }

alt :: { Alt Name Type}
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


param_list :: { [(Located Name, Type)] }
  : singleton_unit_param_list { $1 }
  | params                    { $1 }

param_list1 :: { [(Located Name, Type)] }
  : singleton_unit_param_list { $1 }
  | params1                   { $1 }

param :: { (Located Name, Type) }
  : "(" LOWER_IDENT ":" type ")" { (toString $2 `withLoc` $2, $4) }

singleton_unit_param_list :: { [(Located Name, Type)] }
  : "()"    { [("_" `withLoc` $1, Unit)] }
  | "(" ")" { [("_" `withLoc` $1, Unit)] }

params :: { [(Located Name, Type)] }
  : {- empty -}  { []    }
  | param params { $1:$2 }

params1 :: { [(Located Name, Type)] }
  : param params { $1:$2 }


------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

ident :: { Located Name }
  : UPPER_IDENT  { toString $1 `withLoc` $1 }
  | LOWER_IDENT  { toString $1 `withLoc` $1 }

label :: { Label }
  : LOWER_IDENT  { toString $1 }

{
-- The monadic parser
data P a = ParseOk a | ParseError String deriving (Show)

instance Monad P where
    ParseOk x      >>= f = f x
    ParseError msg >>= f = ParseError msg
    return x         = ParseOk x

instance Functor P where
  fmap  = liftM

instance Applicative P where
  pure  = return
  (<*>) = ap

parseError :: Located Token -> Alex a
parseError (L loc _) = alexError ("Parse error at " ++ show (line loc) ++ ":" ++ show (column loc))

fromHappyParser happyParser source
  = case (runAlex source happyParser) of
      Left  message -> ParseError message
      Right result  -> return result

parseProgram = fromHappyParser happyParseProgram
parseExpr    = fromHappyParser happyParseExpr

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
