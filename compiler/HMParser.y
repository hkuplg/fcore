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
    | let rec var "=" Exp in Exp  { ELetRec $3 $5 $7 }

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

var : lowid  { $1 }

{
parseError :: [HMToken] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

readHM :: String -> Exp
readHM = parseHM . lexHM
}
