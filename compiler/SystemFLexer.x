{
module SystemFLexer where

import SystemFTokens
}

%wrapper "basic"

$upperAlpha = [A-Z]
$lowerAlpha = [a-z]
$alpha = [$upperAlpha $lowerAlpha]
$digit = [0-9]

tokens :-

    $white+     ;
    "--".*      ;

    "/\\"       { \_ -> TokenTLambda }
    "\\"        { \_ -> TokenLambda }
    "->"        { \_ -> TokenArrow }
    "."         { \_ -> TokenDot }
    ":"         { \_ -> TokenColon }
    "("         { \_ -> TokenParenL }
    ")"         { \_ -> TokenParenR }

    [$alpha] [$alpha $digit]* { \s -> TokenId s }

{
}
