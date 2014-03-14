{
module SystemFParser where

import SystemFTokens
import SystemFLexer
import SystemFJava

import Data.Maybe       (fromJust)

}

%name parseSF
%tokentype  { SystemFToken }
%error      { parseError }

%token

"/\\"  { TokenTLambda }
"\\"   { TokenLambda }
"."    { TokenDot }
"->"   { TokenArrow }
":"    { TokenColon }
let    { TokenLet }
"="    { TokenEQ }
in     { TokenIn }
"("    { TokenOParen }
")"    { TokenCParen }
forall { TokenForall }
Int    { TokenInt }
var    { TokenLowId $$ }
tvar   { TokenUpId $$ }

%left TokenArrow

%%

-- data PFExp t e = FVar e
--                | FBLam (t -> PFExp t e)
--                | FLam (PFTyp t) (e -> PFExp t e)
--                | FApp (PFExp t e) (PFExp t e)
--                | FTApp (PFExp t e) (PFTyp t)
Exp : var                               { \(_, env)    -> FVar (fromJust (lookup $1 env)) }
    | "/\\" tvar "." Exp                { \(tenv, env) -> FBLam (\a -> $4 (($2, a):tenv, env)) }
    | "\\" "(" var ":" Typ ")" "." Exp  { \(tenv, env) -> FLam ($5 tenv) (\x -> $8 (tenv, ($3, x):env)) }
    -- let x = e : T in f  ~>  (\(x : T) . f) e
    | let var "=" Exp ":" Typ in Exp    { \(tenv, env) ->
                                            let lambda = \(tenv, env) -> FLam ($6 tenv) (\x -> $8 (tenv, ($2, x):env)) in
                                            FApp (lambda (tenv, env)) ($4 (tenv, env))
                                        }
    | Exp Exp                           { \(tenv, env) -> FApp  ($1 (tenv, env)) ($2 (tenv, env)) }
    | Exp Typ                           { \(tenv, env) -> FTApp ($1 (tenv, env)) ($2 tenv) }
    | "(" Exp ")"                       { $2 }

-- data PFTyp t = FTVar t | FForall (t -> PFTyp t) | FFun (PFTyp t) (PFTyp t) | PFInt
Typ : tvar                 { \tenv -> FTVar (fromJust (lookup $1 tenv)) }
    | forall tvar "." Typ  { \tenv -> FForall (\a -> $4 (($2, a):tenv)) }
    | Typ "->" Typ      { \tenv -> FFun ($1 tenv) ($3 tenv) }
    | Int               { \_    -> PFInt }
    | "(" Typ ")"       { $2 }

{
parseError :: [SystemFToken] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

readSF :: String -> PFExp t e
readSF = (\parser -> parser emptyEnvs) . parseSF . lexSF
    where emptyEnvs = ([], [])

}
