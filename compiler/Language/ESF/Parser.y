{
{-# LANGUAGE RecordWildCards #-}

module Language.ESF.Parser
  ( parseESF
  , readESF
  ) where

import qualified Language.Java.Syntax as J (Op (..))

import Language.ESF.Syntax
import Language.ESF.Lexer
}

%name parseESF
%tokentype { Token }
%monad     { P }
%error     { parseError }

%token

  "("      { Toparen }
  ")"      { Tcparen }
  "/\\"    { Ttlam }
  "\\"     { Tlam }
  ":"      { Tcolon }
  "forall" { Tforall }
  "->"     { Tarrow }
  "."      { Tdot }
  "let"    { Tlet }
  "rec"    { Trec }
  "="      { Teq }
  "and"    { Tand }
  "in"     { Tin }
  "Int"    { Tint }
  "if0"    { Tif0 }
  "then"   { Tthen }
  "else"   { Telse }
  ","      { Tcomma }

  UPPERID  { Tupperid $$ }
  LOWERID  { Tlowerid $$ }
  UNDERID  { Tunderid $$ }

  INTEGER  { Tinteger $$ }

  JAVATYPE { Tjavatype $$ }

  "new"    { Tnew }

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

expr :: { Expr }
     : infixexpr %prec EOF      { $1 }

infixexpr :: { Expr }
    : expr10                    { $1 }
    | infixexpr "*"  infixexpr  { PrimOp J.Mult   $1 $3 }
    | infixexpr "/"  infixexpr  { PrimOp J.Div    $1 $3 }
    | infixexpr "%"  infixexpr  { PrimOp J.Rem    $1 $3 }
    | infixexpr "+"  infixexpr  { PrimOp J.Add    $1 $3 }
    | infixexpr "-"  infixexpr  { PrimOp J.Sub    $1 $3 }
    | infixexpr "<"  infixexpr  { PrimOp J.LThan  $1 $3 }
    | infixexpr "<=" infixexpr  { PrimOp J.LThanE $1 $3 }
    | infixexpr ">"  infixexpr  { PrimOp J.GThan  $1 $3 }
    | infixexpr ">=" infixexpr  { PrimOp J.GThanE $1 $3 }
    | infixexpr "==" infixexpr  { PrimOp J.Equal  $1 $3 }
    | infixexpr "!=" infixexpr  { PrimOp J.NotEq  $1 $3 }
    | infixexpr "&&" infixexpr  { PrimOp J.CAnd   $1 $3 }
    | infixexpr "||" infixexpr  { PrimOp J.COr    $1 $3 }

expr10 :: { Expr }
    : "/\\" tvars "." expr                   { BLam $2 $4  }
    | "\\" pats_with_annot "." expr          { Lam $2 $4 }
    | "let" recflag and_localbinds "in" expr { Let $2 $3 $5 }
    | "if0" expr "then" expr "else" expr     { If0 $2 $4 $6 }
    | "-" INTEGER %prec UMINUS               { Lit (Integer (-$2)) }
    | fexp                                   { $1 }
    -- Java new Object
    | "new" JAVATYPE "(" comma_exprs_emp ")" { JNewObj $2 $4 }

fexp :: { Expr }
    : fexp aexp         { App  $1 $2 }
    | fexp typ          { TApp $1 $2 }
    | aexp              { $1 }

aexp :: { Expr }
    : aexp1             { $1 }

aexp1 :: { Expr }
    : aexp2             { $1 }

aexp2 :: { Expr }
    : var                                       { Var $1 }
    | INTEGER                                   { Lit (Integer $1) }
    | aexp "." UNDERID                          { Proj $1 $3 }
    -- Java method call
    | aexp "." LOWERID "(" comma_exprs_emp ")"  { JMethod $1 $3 $5 }
    | "(" comma_exprs ")"                       { Tuple $2 }
    | "(" expr ")"                              { $2 }

comma_exprs_emp :: { [Expr] }
    : {- empty -}   { []   }
    | expr          { [$1] }
    | comma_exprs   { $1   }

comma_exprs :: { [Expr] }
    : expr "," expr             { [$1, $3] }
    | expr "," comma_exprs      { $1:$3    }

pat :: { Pat }
  : var                 { VarPat $1 }
  -- | "(" comma_pats ")"  { TuplePat $2 }
  | "(" pat ")"         { $2 }

-- comma_pats :: { [Pat] }
--   : pat "," pat         { [$1, $3] }
--   | pat "," comma_pats  { $1:$3    }

pat_with_annot :: { (Pat, Typ) }
  : "(" pat ":" typ ")"         { ($2, $4) }
  | "(" pat_with_annot ")"      { $2       }

pats_with_annot :: { [(Pat, Typ)] }
  : pat_with_annot                      { [$1]  }
  | pat_with_annot pats_with_annot      { $1:$2 }

localbind :: { LocalBind }
    : var tvars_emp var_annots_emp maybe_sig "=" expr
        { LocalBind { local_id     = $1
                    , local_targs  = $2
                    , local_args   = $3
                    , local_rettyp = $4
                    , local_rhs    = $6
                    }
        }

maybe_sig :: { Maybe Typ }
  : ":" typ     { Just $2 }
  | {- empty -} { Nothing }

and_localbinds :: { [LocalBind] }
    : localbind                      { [$1] }
    | localbind "and" and_localbinds { $1:$3    }

recflag :: { RecFlag }
  : "rec"       { Rec }
  | {- empty -} { NonRec }

typ :: { Typ }
    : "forall" tvars "." typ       { Forall $2 $4 }

    -- Require an atyp on the LHS so that `for A. A -> A` cannot be parsed
    -- as `(for A. A) -> A`, since `for A. A` is not a valid atyp.
    | atyp "->" typ     { Fun $1 $3 }

    | atyp              { $1 }

atyp :: { Typ }
    : tvar                      { TVar $1 }
    | "Int"                     { Int }
    | JAVATYPE                  { JTyp $1 }
    | "(" typ ")"               { $2 }
    | "(" comma_typs ")" { Product $2 }

comma_typs :: { [Typ] }
    : typ "," typ               { $1:[$3] }
    | typ "," comma_typs        { $1:$3   }

var :: { String }
    : LOWERID           { $1 }

tvar :: { String }
    : UPPERID           { $1 }

tvars_emp :: { [String] }
  : {- empty -}         { []    }
  | tvar tvars_emp      { $1:$2 }

tvars :: { [String] }
  : tvar tvars_emp  { $1:$2 }

var_annot :: { (Pat, Typ) }
    : "(" var ":" typ ")"  { (VarPat $2, $4) }

var_annots_emp :: { [(Pat, Typ)] }
    : {- empty -}              { []    }
    | var_annot var_annots_emp { $1:$2 }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

parseError :: [Token] -> P a
parseError tokens = PError ("Parse error before tokens:\n\t" ++ show tokens)

readESF :: String -> Expr
readESF src = case (parseESF . lexESF) src of
                 POk expr   -> expr
                 PError msg -> error msg
}
