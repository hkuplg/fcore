{
module HMLexer (lexHM) where

import HMSyntax
import HMTokens
}

%wrapper "basic"

$alpha = [A-Za-z]
$digit = [0-9]

tokens :-

    $white+     ;

    let         { \_ -> TokenLet }
    rec         { \_ -> TokenRec }
    and         { \_ -> TokenAnd }
    \=          { \_ -> TokenEQ }
    in          { \_ -> TokenIn }

    \\          { \_ -> TokenLambda }
    \-\>        { \_ -> TokenArrow }

    \(          { \_ -> TokenOParen }
    \)          { \_ -> TokenCParen }

    \!          { \_ -> TokenUn Not }

    \+          { \_ -> TokenBin Add }
    \-          { \_ -> TokenBin Sub }
    \*          { \_ -> TokenBin Mul }
    \/          { \_ -> TokenBin Div }
    \%          { \_ -> TokenBin Mod }

    \=\=        { \_ -> TokenBin Eq }
    \!\=        { \_ -> TokenBin Ne }
    \<          { \_ -> TokenBin Lt }
    \>          { \_ -> TokenBin Gt }
    \<=         { \_ -> TokenBin Le }
    \>=         { \_ -> TokenBin Ge }

    \&\&        { \_ -> TokenBin And }
    \|\|        { \_ -> TokenBin Or }

    if          { \_ -> TokenIf }
    then        { \_ -> TokenThen }
    else        { \_ -> TokenElse }

    $digit+     { \s -> TokenInt (read s) }

    [a-z] [$alpha $digit \_ \']*  { \s -> TokenLowId s }
    [A-Z] [$alpha $digit \_ \']*  { \s -> TokenUpId s }

{
lexHM :: String -> [HMToken]
lexHM = alexScanTokens
}
