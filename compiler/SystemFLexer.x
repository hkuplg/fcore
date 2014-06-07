{
module SystemFLexer (lexSF) where

import SystemFTokens

import Language.Java.Syntax as J

}

%wrapper "basic"

$alpha = [A-Za-z]
$digit = [0-9]

tokens :-

    $white+     ;

    \/\\        { \_ -> TokenTLambda }
    \\          { \_ -> TokenLambda }
    \-\>        { \_ -> TokenArrow }
    \.          { \_ -> TokenDot }
    \:          { \_ -> TokenColon }
    \,          { \_ -> TokenComma }
    let         { \_ -> TokenLet }
    \=          { \_ -> TokenEQ }
    in          { \_ -> TokenIn }
    \(          { \_ -> TokenOParen }
    \)          { \_ -> TokenCParen }
    forall      { \_ -> TokenForall }
    Int         { \_ -> TokenIntType }
    if0         { \_ -> TokenIf0 }
    then        { \_ -> TokenThen }
    else        { \_ -> TokenElse }
    fix         { \_ -> TokenFix }

    -- http://hackage.haskell.org/package/language-java-0.2.5/docs/src/Language-Java-Syntax.html#Op
    \*          { \_ -> TokenPrimOp J.Mult }
    \/          { \_ -> TokenPrimOp J.Div }
    \%          { \_ -> TokenPrimOp J.Rem }
    \+          { \_ -> TokenPrimOp J.Add }
    \-          { \_ -> TokenPrimOp J.Sub }
    \<          { \_ -> TokenPrimOp J.LThan }
    \>          { \_ -> TokenPrimOp J.GThan }
    \<\=        { \_ -> TokenPrimOp J.LThanE }
    \>\=        { \_ -> TokenPrimOp J.GThanE }
    \=\=        { \_ -> TokenPrimOp J.Equal }
    \!\=        { \_ -> TokenPrimOp J.NotEq }
    \&\&        { \_ -> TokenPrimOp J.And }
    \|\|        { \_ -> TokenPrimOp J.Or }

    [a-z] [$alpha $digit \_ \']*  { \s -> TokenLowId s }
    [A-Z] [$alpha $digit \_ \']*  { \s -> TokenUpId s }

    $digit+    { \s -> TokenInt (read s) }

    \_ $digit+ { \s -> TokenTupleField (read (tail s))  }

{
lexSF :: String -> [SystemFToken]
lexSF = alexScanTokens
}
