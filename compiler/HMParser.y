{
module HMParser where

import HMTokens
import HMLexer
import HM       (Exp (..), BinOp (..), CompOp (..))

}

%name parseHM
%tokentype  { HMToken }
%error      { parseError }

%token

let     { TokenLet }
rec     { TokenRec }
and     { TokenAnd }
"="     { TokenEQ }
in      { TokenIn }

"\\"    { TokenLambda }
"->"    { TokenArrow }

"("     { TokenOParen }
")"     { TokenCParen }

"+"     { TokenBin Add }
"-"     { TokenBin Sub }
"*"     { TokenBin Mul }
"/"     { TokenBin Div }
"%"     { TokenBin Mod }

"=="    { TokenComp Eq }
"/="    { TokenComp Ne }
"<"     { TokenComp Lt }
">"     { TokenComp Gt }
"<="    { TokenComp Le }
">="    { TokenComp Ge }

if      { TokenIf }
then    { TokenThen }
else    { TokenElse }

int     { TokenInt $$ }

lowid   { TokenLowId $$ }

%right "in"
%left "->"

%left "||"
%left "&&"
%left "==" "/=" "<" ">" "<=" ">="

%%

Exp : "(" Exp ")"             { $2 }
    | var                     { EVar $1 }
    | Exp Exp                 { EApp $1 $2 }
    | "\\" var "->" Exp       { ELam $2 $4 }
    | let     var "=" Exp in Exp  { ELet    $2 $4 $6 }
    | let rec Bindings    in Exp  { ELetRec $3 $5 }

    | Exp "+" Exp  { EBin Add $1 $3 }
    | Exp "-" Exp  { EBin Sub $1 $3 }
    | Exp "*" Exp  { EBin Mul $1 $3 }
    | Exp "/" Exp  { EBin Div $1 $3 }
    | Exp "%" Exp  { EBin Mod $1 $3 }

    | Exp "==" Exp  { EComp Eq $1 $3 }
    | Exp "/=" Exp  { EComp Ne $1 $3 }
    | Exp "<"  Exp  { EComp Lt $1 $3 }
    | Exp ">"  Exp  { EComp Gt $1 $3 }
    | Exp "<=" Exp  { EComp Le $1 $3 }
    | Exp ">=" Exp  { EComp Ge $1 $3 }

    | if Exp then Exp else Exp  { EIf $2 $4 $6 }
    | int  { EInt $1 }

Binding : var "=" Exp   { ($1, $3) }

Bindings : Binding               { [$1] }
         | Bindings and Binding  { $1 ++ [$3] }

var : lowid  { $1 }

{
parseError :: [HMToken] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

readHM :: String -> Exp
readHM = parseHM . lexHM

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> n == 1 || even (n-1) in odd 10"

}
