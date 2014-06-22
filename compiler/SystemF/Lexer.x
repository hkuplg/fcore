{
module SystemF.Lexer (lexer, Token (..)) where

import Language.Java.Syntax     as JS    (Op (..))
}

%wrapper "basic"

$alpha = [A-Za-z]
$digit = [0-9]

tokens :-

    $white+     ;

    \(          { \_ -> OParen }
    \)          { \_ -> CParen }
    \/\\        { \_ -> TLam }
    \\          { \_ -> Lam }
    \:          { \_ -> Colon }
    forall      { \_ -> Forall }
    \-\>        { \_ -> Arrow }
    \.          { \_ -> Dot }
    let         { \_ -> Let }
    \=          { \_ -> Eq }
    in          { \_ -> In }
    fix         { \_ -> Fix }
    Int         { \_ -> TypeInt }
    if0         { \_ -> If0 }
    then        { \_ -> Then }
    else        { \_ -> Else }
    \,          { \_ -> Comma }

    -- http://hackage.haskell.org/package/language-java-0.2.5/docs/src/Language-Java-Syntax.html#Op
    \*          { \_ -> Op3 JS.Mult }
    \/          { \_ -> Op3 JS.Div }
    \%          { \_ -> Op3 JS.Rem }
    \+          { \_ -> Op4 JS.Add }
    \-          { \_ -> Op4 JS.Sub }
    \<          { \_ -> Op6 JS.LThan }
    \>          { \_ -> Op6 JS.GThan }
    \<\=        { \_ -> Op6 JS.LThanE }
    \>\=        { \_ -> Op6 JS.GThanE }
    \=\=        { \_ -> Op7 JS.Equal }
    \!\=        { \_ -> Op7 JS.NotEq }
    \&\&        { \_ -> Op11 JS.CAnd }
    \|\|        { \_ -> Op12 JS.COr }

    [a-z] [$alpha $digit \_ \']*  { \s -> LowId s }
    [A-Z] [$alpha $digit \_ \']*  { \s -> UpId s }

    $digit+    { \s -> Int (read s) }

    \_ $digit+ { \s -> UnderId (read (tail s))  }

{

data Token = OParen | CParen
           | TLam | Lam | Colon | Forall | Arrow | Dot
           | Let | Eq | In | Fix
           | TypeInt
           | If0 | Then | Else
           | Comma
           | Op3 JS.Op | Op4 JS.Op | Op6 JS.Op | Op7 JS.Op | Op11 JS.Op | Op12 JS.Op
           | LowId String | UpId String | UnderId Int
           | Int Integer
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
