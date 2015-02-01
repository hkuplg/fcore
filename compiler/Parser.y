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
  "'"      { Tquote }
  "type"   { Ttype }
  "let"    { Tlet }
  "rec"    { Trec }
  "="      { Teq }
  "and"    { Tand }
  "in"     { Tin }
  "if"     { Tif }
  "then"   { Tthen }
  "else"   { Telse }
  ","      { Tcomma }

  "data"   { Tdata }
  "case"   { Tcase }
  "|"      { Tbar }
  "of"     { Tof }

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
  Empty    { Temptytree }
  Fork     { Tnonemptytree}
  CHAR     { Tchar $$ }
  "()"     { Tunitlit }
  "Unit"   { Tunit }
  List     { Tlist }
  head     { Tlisthead }
  tail     { Tlisttail }
  cons     { Tlistcons }
  isNil    { Tlistisnil }
  length   { Tlistlength }

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
module :: { ReaderModule }
  : "module" module_name semi_binds "end"  { Module $2 $3 }

module_name :: { ReaderId }
  : UPPERID  { $1 }

constr_name :: { ReaderId }
  : tvar  { $1 }

-- Types

type :: { ReaderType }
  : "forall" tvar "." type   { Forall $2 $4 }
  | monotype                 { $1 }

monotype :: { ReaderType }
  : intertype "->" monotype  { Fun $1 $3 }
  | intertype                { $1 }

intertype :: { ReaderType }
  : ftype "&" intertype      { And $1 $3 }
  | ftype                    { $1 }

ftype :: { ReaderType }
  : ftype atype              { OpApp $1 $2 }
  | atype                    { $1 }

atype :: { ReaderType }
  : tvar                     { TVar $1 }
  | JAVACLASS                { JType (JClass $1) }
  | "Unit"                   { Unit }
  | "(" product_body ")"     { Product $2 }
  -- TODO: desugaring might be too early. But the benefit is avoid a traversal of the type structure later.
  | "{" record_body "}"      { foldl (\ acc (l,t) -> And acc (Record [(l,t)])) (Record [(head $2)]) (tail $2) }
  | "'" atype                { Thunk $2 }
  | "(" type ")"             { $2 }
  | List "<" type ">"        { ListOf $3}

product_body :: { [ReaderType] }
  : type "," type             { $1:[$3] }
  | type "," product_body     { $1:$3   }

record_body :: { [(Label, ReaderType)] }
  : label ":" type                  { [($1, $3)]  }
  | label ":" type "," record_body  { ($1, $3):$5 }

label :: { Label }
  : LOWERID                  { $1 }

tvars :: { [ReaderId] }
  : {- empty -}              { []    }
  | tvar tvars               { $1:$2 }

tvar :: { ReaderId }
  : UPPERID                  { $1 }

vars :: { [ReaderId] }
  : {- empty -}              { []    }
  | var vars                 { $1:$2 }

types :: { [Type] }
  : {- empty -}              { [] }
  | atype types              { $1:$2 }

-- Expressions

expr :: { ReaderExpr }
    : "/\\" tvar "." expr                 { BLam $2 $4  }
    | "\\" arg "." expr                   { Lam $2 $4 }
    | "let" recflag and_binds "in" expr   { Let $2 $3 $5 }
    | "let" recflag and_binds ";"  expr   { Let $2 $3 $5 }
    | "let"  tvar tvars "=" type "in" expr { Type $2 $3 $5 $7 }
    | "let"  tvar tvars "=" type ";"  expr { Type $2 $3 $5 $7 }

    -- Type synonyms
    | "type" tvar tvars "=" type "in" expr { Type $2 $3 $5 $7 }
    | "type" tvar tvars "=" type ";"  expr { Type $2 $3 $5 $7 }

    | "if" expr "then" expr "else" expr   { If $2 $4 $6 }
    | "-" INT %prec UMINUS                { Lit (Int (-$2)) }
    | "data" tvar "=" constrs_decl ";" expr    { Data $2 $4 $6 }
    | "case" expr "of" patterns           { Case $2 $4 }
    | infixexpr                           { $1 }
    | module expr                   { LetModule $1 $2 }

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
    : fexpr aexpr        { App  $1 $2 }
    | fexpr atype        { TApp $1 $2 }
    | aexpr              { $1 }

aexpr :: { ReaderExpr }
    : var                       { Var $1 }
    | lit                       { $1 }
    | "(" comma_exprs2 ")"      { Tuple $2 }
    | aexpr "." UNDERID         { Proj $1 $3 }
    | module_name "." var       { ModuleAccess $1 $3 }
    | javaexpr                  { $1 }
    | "{" semi_exprs "}"        { Seq $2 }
    | "{" recordlit_body "}"    { RecordIntro $2 }
    | aexpr "with" "{" recordlit_body "}"  { RecordUpdate $1 $4 }
    | "new" List "<" type ">" "()"  {PolyList [] $4}
    | "new" List "<" type ">" "(" comma_exprs0 ")"  {PolyList $7 $4}
    | head "(" fexpr ")"              { JProxyCall (JMethod (NonStatic $3 ) "head" [] undefined) undefined}
    | tail "(" fexpr ")"              { JProxyCall (JMethod (NonStatic $3 ) "tail" [] undefined) undefined}
    | isNil "(" fexpr ")"             { JMethod (NonStatic $3) "isEmpty" [] undefined}
    | length "(" fexpr ")"            { JMethod (NonStatic $3) "length" [] undefined}
    | cons "(" fexpr "," fexpr ")"    { JProxyCall (JNew "f2j.FunctionalList" [$3,$5]) undefined}
    | "{" constr_name aexprs "}"{ Constr (Constructor $2 []) $3 }
    | "(" expr ")"              { $2 }

lit :: { ReaderExpr }
    : INT                       { Lit (Int $1)    }
    | STRING                    { Lit (String $1) }
    | BOOL                      { Lit (Bool $1)   }
    | CHAR                      { Lit (Char $1)   }
    | "()"                      { Lit UnitLit     }

javaexpr :: { ReaderExpr }
    : "new" JAVACLASS "(" comma_exprs0 ")"        { JNew $2 $4 }
    | Empty                                       { JNew "f2j.FunctionalTree" [] }
    | Fork "(" comma_exprs0 ")"                   { JNew "f2j.FunctionalTree" $3}

    | JAVACLASS "." LOWERID "(" comma_exprs0 ")"  { JMethod (Static $1) $3 $5 undefined }
    | JAVACLASS "." LOWERID "()"                  { JMethod (Static $1) $3 [] undefined }
    | JAVACLASS "." LOWERID                       { JField  (Static $1) $3 undefined }
    | JAVACLASS "." UPPERID                       { JField  (Static $1) $3 undefined } -- Constants

    -- A dot can mean three things:
    -- (1) method invocation
    -- (2) field access
    -- (3) record elimination

    -- case length comma_exprs0
    -- when 0: method invocation
    --   (since the gap between the two parentheses distinguishes the string from the unit literal `()`)
    -- when 1: method invocation or application (with a parenthesized argument)
    -- else:   method invocation or application (with a tuple)
    | aexpr "." LOWERID "(" comma_exprs0 ")"  { Dot $1 $3 (Just ($5, UnitImpossible)) }

    -- method invocation or application
    | aexpr "." LOWERID "()"                  { Dot $1 $3 (Just ([], UnitPossible)) }

    -- field access or record elimination
    | aexpr "." LOWERID                       { Dot $1 $3 Nothing }

    -- Is this possible?
    -- | aexpr "." UPPERID                    { Dot $1 $3 Nothing }

recordlit_body :: { [(Label, ReaderExpr)] }
  : label "=" expr                     { [($1, $3)]  }
  | label "=" expr "," recordlit_body  { ($1, $3):$5 }

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

aexprs :: { [ReaderExpr] }
    :  {- empty -}            { [] }
    | aexpr aexprs            { $1:$2 }

bind :: { ReaderBind }
    : var tvars args maybe_sig "=" expr
        { Bind { bindId       = $1
               , bindTargs    = $2
               , bindArgs     = $3
               , bindRhs      = $6
               , bindRhsAnnot = $4
               }
        }

maybe_sig :: { Maybe ReaderType }
  : ":" type     { Just $2 }
  | {- empty -} { Nothing }

and_binds :: { [ReaderBind] }
    : bind                      { [$1]  }
    | bind "and" and_binds      { $1:$3 }

semi_binds :: { [ReaderBind] }
    : bind                      { [$1]  }
    | bind ";" and_binds      { $1:$3 }

recflag :: { RecFlag }
  : "rec"       { Rec }
  | {- empty -} { NonRec }

arg :: { (ReaderId, ReaderType) }
    : "(" var ":" type ")"       { ($2, $4) }
    | "()"                       { ("_", Unit) }
    | "(" arg ")"                { $2 }

args :: { [(ReaderId, ReaderType)] }
    : {- empty -}               { []    }
    | arg args  { $1:$2 }

var :: { ReaderId }
    : LOWERID           { $1 }

constrs_decl :: { [Constructor] }
    : constr_decl { [$1] }
    | constr_decl "|" constrs_decl { $1:$3 }

constr_decl :: { Constructor }
    : constr_name types { Constructor $1 $2 }

patterns :: { [Alt ReaderId Type] }
    : pattern { [$1] }
    | pattern "|" patterns { $1:$3 }

pattern :: { Alt ReaderId Type}
    : constr_name vars "->" expr { ConstrAlt (Constructor $1 []) $2 $4 }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

parseError :: [Token] -> P a
parseError tokens = PError ("Parse error before tokens:\n\t" ++ show tokens)

reader :: String -> ReaderExpr
reader src = case (parseExpr . lexer) src of
                 POk expr   -> expr
                 PError msg -> error msg
}
