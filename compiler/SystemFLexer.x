{
module SystemFLexer (lexSF) where

import SystemFTokens

}

%wrapper "basic"

$upperAlpha = [A-Z]
$lowerAlpha = [a-z]
$alpha = [$upperAlpha $lowerAlpha]
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

    [a-z] [$alpha $digit \_ \']*        { \s -> TokenId s }
    [\'] [a-z] [$alpha $digit \_ \']*   { \s -> TokenTVar (drop 1 s) }

{
lexSF :: String -> [SystemFToken]
lexSF = alexScanTokens
}
