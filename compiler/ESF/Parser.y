{
{-# LANGUAGE RecordWildCards #-}

module ESF.Parser where

import qualified Language.Java.Syntax as J (Op (..))

import ESF.Syntax
import ESF.Lexer
}

%name parser
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

term :: { Expr }
     : infixterm %prec EOF      { $1 }

infixterm :: { Expr }
    : term10                    { $1 }
    | infixterm "*"  infixterm  { PrimOp $1 J.Mult   $3 }
    | infixterm "/"  infixterm  { PrimOp $1 J.Div    $3 }
    | infixterm "%"  infixterm  { PrimOp $1 J.Rem    $3 }
    | infixterm "+"  infixterm  { PrimOp $1 J.Add    $3 }
    | infixterm "-"  infixterm  { PrimOp $1 J.Sub    $3 }
    | infixterm "<"  infixterm  { PrimOp $1 J.LThan  $3 }
    | infixterm "<=" infixterm  { PrimOp $1 J.LThanE $3 }
    | infixterm ">"  infixterm  { PrimOp $1 J.GThan  $3 }
    | infixterm ">=" infixterm  { PrimOp $1 J.GThanE $3 }
    | infixterm "==" infixterm  { PrimOp $1 J.Equal  $3 }
    | infixterm "!=" infixterm  { PrimOp $1 J.NotEq  $3 }
    | infixterm "&&" infixterm  { PrimOp $1 J.CAnd   $3 }
    | infixterm "||" infixterm  { PrimOp $1 J.COr    $3 }

term10 :: { Expr }
    : "/\\" tvar "." term                 { BLam $2 $4  }
    | "\\" var_with_annot "." term        { Lam $2 $4 }
    | "let" recflag and_binds "in" term   { Let $2 $3 $5 }
    | "if0" term "then" term "else" term  { If0 $2 $4 $6 }
    | "-" INTEGER %prec UMINUS            { Lit (Integer (-$2)) }
    | fexp                                { $1 }

fexp :: { Expr }
    : fexp aexp         { App  $1 $2 }
    | fexp typ          { TApp $1 $2 }
    | aexp              { $1 }

aexp :: { Expr }
    : aexp1             { $1 }

aexp1 :: { Expr }
    : aexp2             { $1 }

aexp2 :: { Expr }
    : var                       { Var $1 }
    | INTEGER                   { Lit (Integer $1) }
    | aexp "." UNDERID          { Proj $1 $3 }
    | "(" comma_terms ")"       { Tuple $2 }
    | "(" term ")"              { $2 }

comma_terms :: { [Expr] }
    : term "," term             { [$1, $3] }
    | term "," comma_terms      { $1:$3    }

var_with_annot :: { (Name, Type) }
  : "(" var ":" typ ")"         { ($2, $4) }
  | "(" var_with_annot ")"      { $2       }

bind :: { Bind }
    : var tvars_emp var_annots_emp maybe_sig "=" term
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

and_binds :: { [Bind] }
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

atyp :: { Type }
    : tvar                      { TyVar $1 }
    | "Int"                     { Int }
    | "(" typ ")"               { $2 }
    | "(" comma_typs ")"        { Product $2 }

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

reader :: String -> Expr
reader src = case (parser . lexer) src of
                 POk term   -> term
                 PError msg -> error msg
}
