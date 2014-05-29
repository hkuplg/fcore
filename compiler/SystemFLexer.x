{
module SystemFLexer (lexSF) where

import SystemFTokens

}

%wrapper "basic"

$alpha = [A-Za-z]
$digit = [0-9]

tokens :-

    $white+     ;

    \/\\        { \_ -> TokenTLambda }
    \\          { \_ -> TokenLambda }
    fix         { \_ -> TokenFix }
    \,          { \_ -> TokenComma }
    \.          { \_ -> TokenDot }
    \-\>        { \_ -> TokenArrow }
    \:          { \_ -> TokenColon }
    let         { \_ -> TokenLet }
    \=          { \_ -> TokenEQ }
    in          { \_ -> TokenIn }
    \(          { \_ -> TokenOParen }
    \)          { \_ -> TokenCParen }
    forall      { \_ -> TokenForall }
    Int         { \_ -> TokenIntType }
    if          { \_ -> TokenIf }
    then        { \_ -> TokenThen }
    else        { \_ -> TokenElse }

    [a-z] [$alpha $digit \_ \']*  { \s -> TokenLowId s }
    [A-Z] [$alpha $digit \_ \']*  { \s -> TokenUpId s }
    [A-Z] [$alpha $digit \_ \']*  { \s -> TokenUpId s }

    [$digit]+  { \s -> TokenInt (read s) }

{
lexSF :: String -> [SystemFToken]
lexSF = alexScanTokens
}
