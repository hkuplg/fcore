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
    \.          { \_ -> TokenDot }
    \-\>        { \_ -> TokenArrow }
    \:          { \_ -> TokenColon }
    \(          { \_ -> TokenOParen }
    \)          { \_ -> TokenCParen }
    forall      { \_ -> TokenForall }
    Int         { \_ -> TokenInt }

    [a-z] [$alpha $digit \_ \']*  { \s -> TokenLowId s }
    [A-Z] [$alpha $digit \_ \']*  { \s -> TokenUpId s }

{
lexSF :: String -> [SystemFToken]
lexSF = alexScanTokens
}
