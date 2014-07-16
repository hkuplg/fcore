{
{-# LANGUAGE RankNTypes, RecordWildCards #-}

module Language.SystemF.Parser where

-- References:
-- http://www.haskell.org/onlinereport/exps.html
-- http://caml.inria.fr/pub/docs/manual-ocaml/expr.html

import Data.Maybe       (fromMaybe)
import qualified Data.Map as Map

import qualified Language.Java.Syntax as J (Op (..))

import Language.SystemF.Syntax
import Language.SystemF.Lexer
import Language.SystemF.TypeCheck       (infer, unsafeGeneralize)
}

%name parser
%tokentype { Token }

-- The lexical analysis function must have the following type:
-- lexer :: (Token -> P a) -> P a
-- %lexer  { lexer }

%monad     { P }
%error     { parseError }

%token

    "("      { OParen }
    ")"      { CParen }
    "/\\"    { TLam }
    "\\"     { Lam }
    ":"      { Colon }
    "forall" { Forall }
    "->"     { Arrow }
    "."      { Dot }
    "let"    { Let }
    "rec"    { Rec }
    "="      { Eq }
    "and"    { And }
    "in"     { In }
    "fix"    { Fix }
    "Int"    { Int }
    "if0"    { If0 }
    "then"   { Then }
    "else"   { Else }
    ","      { Comma }

    UPPERID  { UpperId $$ }
    LOWERID  { LowerId $$ }
    UNDERID  { UnderId $$ }

    INTEGER  { Integer $$ }

    "*"      { PrimOp J.Mult   }
    "/"      { PrimOp J.Div    }
    "%"      { PrimOp J.Rem    }
    "+"      { PrimOp J.Add    }
    "-"      { PrimOp J.Sub    }
    "<"      { PrimOp J.LThan  }
    "<="     { PrimOp J.LThanE }
    ">"      { PrimOp J.GThan  }
    ">="     { PrimOp J.GThanE }
    "=="     { PrimOp J.Equal  }
    "!="     { PrimOp J.NotEq  }
    "&&"     { PrimOp J.CAnd   }
    "||"     { PrimOp J.COr    }

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

-- Reference for rules:
-- https://github.com/ghc/ghc/blob/master/compiler/parser/Parser.y.pp#L1453

exp -- :: { forall t e. Exp t e }
    : infixexp %prec EOF        { $1 }

infixexp -- :: { forall t e. Exp t e }
    : exp10                     { $1 }
    | infixexp "*"  infixexp    { \e -> FPrimOp ($1 e) J.Mult   ($3 e) }
    | infixexp "/"  infixexp    { \e -> FPrimOp ($1 e) J.Div    ($3 e) }
    | infixexp "%"  infixexp    { \e -> FPrimOp ($1 e) J.Rem    ($3 e) }
    | infixexp "+"  infixexp    { \e -> FPrimOp ($1 e) J.Add    ($3 e) }
    | infixexp "-"  infixexp    { \e -> FPrimOp ($1 e) J.Sub    ($3 e) }
    | infixexp "<"  infixexp    { \e -> FPrimOp ($1 e) J.LThan  ($3 e) }
    | infixexp "<=" infixexp    { \e -> FPrimOp ($1 e) J.LThanE ($3 e) }
    | infixexp ">"  infixexp    { \e -> FPrimOp ($1 e) J.GThan  ($3 e) }
    | infixexp ">=" infixexp    { \e -> FPrimOp ($1 e) J.GThanE ($3 e) }
    | infixexp "==" infixexp    { \e -> FPrimOp ($1 e) J.Equal  ($3 e) }
    | infixexp "!=" infixexp    { \e -> FPrimOp ($1 e) J.NotEq  ($3 e) }
    | infixexp "&&" infixexp    { \e -> FPrimOp ($1 e) J.CAnd   ($3 e) }
    | infixexp "||" infixexp    { \e -> FPrimOp ($1 e) J.COr    ($3 e) }

exp10 -- :: { forall t e. Exp t e }
    : "/\\" tvar "." exp                { \(tenv, env, i) -> FBLam (\a -> $4 (Map.insert $2 a tenv, env, i)) }
    | "\\" "(" var ":" typ ")" "." exp  { \(tenv, env, i) -> FLam ($5 tenv) (\x -> $8 (tenv, Map.insert $3 x env, i)) }

    -- let x = e1 : T in e2 ~~> (\(x : T). e2) e1

    | "let" var{-2-} "=" exp{-4-} ":" typ{-6-} "in" exp{-8-}
        { \(tenv, env, i) -> FApp (FLam ($6 tenv) (\x -> $8 (tenv, Map.insert $2 x env, i))) ($4 (tenv, env, i)) }

    -- let x = e1 in e2 ~~> (\(x : (infer e1)). e2) e1

    | "let" var{-2-} "=" exp{-4-} "in" exp{-6-}
        { \(tenv, env, i) ->
            let e1 = $4 (tenv, env, i) in
            FApp (unsafeGeneralize $ FLam (infer i e1) (\x -> $6 (tenv, Map.insert $2 x env, i))) e1
        }

    {-
    let rec f A1 ... An (x1 : T1) ... (xn : Tn) : T(n+1) = e1 in e2
~~> let f = /\A1. ... /\An. (fix (f : T1 -> ... -> Tn -> T(n+1)). \x1. (\(x2 : T2). ... \(xn : Tn). e1)) in e2
~~> (\(f : (infer e3). e2) e3
    -}

    | "let" "rec" letbinding{-3-} "in" exp{-5-}
        { \(tenv, env, i) ->
            let LetBinding {..} = $3 in
            let e3 = (withBLams lbtvars (\(tenv, env, i) ->
                        FFix
                            (\y -> \x -> (withLams tenv lbvar_annots lbexp)
                                (tenv, (Map.insert (fst lbvar_annot) x . Map.insert lbvar y) env, i))
                            ((snd lbvar_annot) tenv)
                            (mkFunType tenv (map snd lbvar_annots ++ [lbtyp]))
                     )) (tenv, env, i)
            in
            FApp (unsafeGeneralize $ FLam (infer i e3) (\f -> $5 (tenv, Map.insert lbvar f env, i))) e3
        }

    {-

    let rec f1 A1_1 ... A1_n (x1_0 : T1_0) (x1_1 : T1_1) ... (x1_n : T1_n) = e1
    and     f2 A2_1 ... A2_n (x2_0 : T2_0) (x2_1 : T2_1) ... (x2_n : T2_n) = e2
    ...
    and     fn An_1 ... An_n (xn_0 : Tn_0) (xn_1 : Tn_1) ... (xn_n : Tn_n) = en
    in
    e

~~> let rec m (dummy : Int) : (sig1, sig2, ..., sign) =
        ( /\A1_1. ... /\A1_n. \(x1_0 : T1_0). \(x1_1 : T1_1). ... \(x1_n : T1_n). e1
        , ...
        , /\An_1. ... /\An_n. \(xn_0 : Tn_0). \(xn_1 : Tn_1). ... \(xn_n : Tn_n). en
        )
    in
    e
    where in the environment:
        f1 |-> (m 0)._0
        ...
        fn |-> (m 0)._(n-1)

    -}

    | "let" "rec" and_letbindings "in" exp { \(tenv, env, i) -> FLit 1 -- TODO }

    | "fix" "(" var ":" atyp{-5-} "->" typ{-7-} ")" "." "\\" var "." exp
        { \(tenv, env, i) ->
            FFix (\y -> \x ->
                    $13 (tenv, (Map.insert $11 x . Map.insert $3 y) env, i))
                ($5 tenv) ($7 tenv)
        }

    | "if0" exp "then" exp "else" exp   { \e -> FIf0 ($2 e) ($4 e) ($6 e) }
    | "-" INTEGER %prec UMINUS          { \e -> FLit (-$2) }
    | fexp                              { $1 }

-- There are times when it is more convenient to parse a more general
-- language than that which is actually intended, and check it later.

letbinding -- :: { forall t e. LetBinding t e }
    : var tvars var_annot var_annots ":" typ "=" exp
        { LetBinding { lbvar = $1
                     , lbtvars = $2
                     , lbvar_annot = $3
                     , lbvar_annots = $4
                     , lbtyp = $6
                     , lbexp = $8
                     }
        }

and_letbindings -- :: { forall t e. [LetBinding t e] }
    : letbinding "and" letbinding       { [$1, $3] }
    | letbinding "and" and_letbindings  { $1:$3    }

fexp -- :: { forall t e. Exp t e }
    : fexp aexp         { \(tenv, env, i) -> FApp  ($1 (tenv, env, i)) ($2 (tenv, env, i)) }
    | fexp typ          { \(tenv, env, i) -> FTApp ($1 (tenv, env, i)) ($2 tenv) }
    | aexp              { $1 }

aexp -- :: { forall t e. Exp t e }
    : aexp1             { $1 }

aexp1 -- :: { forall t e. Exp t e }
    : aexp2             { $1 }

aexp2 -- :: { forall t e. Exp t e }
    : var                       { \(tenv, env, i) -> FVar $1 (fromMaybe (error $ "Unbound variable: `" ++ $1 ++ "'") (Map.lookup $1 env)) }
    | INTEGER                   { \_e -> FLit $1 }
    | aexp "." UNDERID          { \e -> FProj $3 ($1 e) }
    | "(" exp ")"               { $2 }
    | "(" comma_exps ")"        { \(tenv, env, i) -> FTuple (map ($ (tenv, env, i)) $2) }

comma_exps -- :: { forall t e. [Exp t e] }
    : exp "," exp               { [$1, $3] }
    | exp "," comma_exps        { $1:$3    }

typ -- :: { forall t. Typ t }
    : "forall" tvar "." typ     { \tenv -> FForall (\a -> $4 (Map.insert $2 a tenv)) }

    -- Require an atyp on the LHS so that `for A. A -> A` cannot be
    -- parsed as `(for A. A) -> A` since `for A. A` is not a valid atyp.
    | atyp "->" typ             { \tenv -> FFun ($1 tenv) ($3 tenv) }

    | atyp                      { $1 }

comma_typs -- :: { forall t. [Typ t] }
    : typ "," typ               { $1:[$3] }
    | typ "," comma_typs        { $1:$3   }

atyp -- :: { forall t. Typ t }
    : tvar                      { \tenv -> FTVar (fromMaybe (error $ "Unbound type variable: `" ++ $1 ++ "'") (Map.lookup $1 tenv)) }
    | "Int"                     { \_    -> FInt }
    | "(" typ ")"               { $2 }
    | "(" comma_typs ")"        { \tenv -> FProduct (map ($ tenv) $2) }

var :: { String }
    : LOWERID          { $1 }

tvar :: { String }
    : UPPERID          { $1 }

tvars :: { [String] }
    : tvar tvars        { $1:$2 }
    | {- empty -}       { []    }

var_annot -- :: { forall t. (String, Typ t) }
    : "(" var ":" typ ")"  { ($2, $4) }

var_annots -- :: { forall t. [(String, Typ t)] }
    : var_annot var_annots      { $1:$2 }
    | {- empty -}               { [] }

{
-- The monadic parser
data P a = POk a | PError String

instance Monad P where
    POk x      >>= f = f x
    PError msg >>= f = PError msg
    return x         = POk x

type TEnv t = Map.Map String t
type Env e  = Map.Map String e

type Exp t e = (TEnv t, Env e, Int) -> PFExp t e
type Typ t   = TEnv t -> PFTyp t

-- "let" var tvars lbvar_annot lbvar_annots ":" lbtyp "=" lbexp
data LetBinding t e = LetBinding
                    { lbvar        :: String
                    , lbtvars      :: [String]
                    , lbvar_annot  :: (String, Typ t)
                    , lbvar_annots :: [(String, Typ t)]
                    , lbtyp        :: Typ t
                    , lbexp        :: Exp t e
                    }

withBLams :: [String] -> Exp t e -> Exp t e
withBLams []     expr = expr
withBLams (a:as) expr = \(tenv, env, i) -> FBLam (\x -> (withBLams as expr) (Map.insert a x tenv, env, i))

withLams :: TEnv t -> [(String, Typ t)]  -> Exp t e -> Exp t e
withLams tenv []              expr = expr
withLams tenv ((var, typ):xs) expr = \(tenv, env, i) -> FLam (typ tenv) (\x -> (withLams tenv xs expr) (tenv, Map.insert var x env, i))

mkFunType :: TEnv t -> [Typ t] -> PFTyp t
mkFunType tenv []     = error "mkFunType: impossible case reached"
mkFunType tenv [t]    = t tenv
mkFunType tenv (t:ts) = FFun (t tenv) (mkFunType tenv ts)

parseError :: [Token] -> P a
parseError tokens = PError $ "Parse error before tokens:\n\t" ++ show tokens

reader :: String -> PFExp t e
reader src =
    let tokens = lexer src in
    case parser tokens of
        POk e      -> unsafeGeneralize (e emptyEnvs)
        PError msg -> error msg

    where emptyEnvs = (Map.empty, Map.empty, 0)
}
