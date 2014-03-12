{
module SystemFLexer (systemFLex) where

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

    [$alpha] [$alpha $digit \_ \']* { \s -> TokenId s }

{
systemFLex :: String -> [SystemFToken]
systemFLex = alexScanTokens
}
