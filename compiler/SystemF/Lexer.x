{
module SystemF.Lexer (lexer, Token (..)) where

import Language.Java.Syntax     as J    (Op (..))
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
    \*          { \_ -> Op J.Mult }
    \/          { \_ -> Op J.Div }
    \%          { \_ -> Op J.Rem }
    \+          { \_ -> Op J.Add }
    \-          { \_ -> Op J.Sub }
    \<          { \_ -> Op J.LThan }
    \>          { \_ -> Op J.GThan }
    \<\=        { \_ -> Op J.LThanE }
    \>\=        { \_ -> Op J.GThanE }
    \=\=        { \_ -> Op J.Equal }
    \!\=        { \_ -> Op J.NotEq }
    \&\&        { \_ -> Op J.And }
    \|\|        { \_ -> Op J.Or }

    [a-z] [$alpha $digit \_ \']*  { \s -> LowId s }
    [A-Z] [$alpha $digit \_ \']*  { \s -> UpId s }

    $digit+    { \s -> Int (read s) }

    \_ $digit+ { \s -> TupleField (read (tail s))  }

{

data Token = OParen | CParen
           | TLam | Lam | Colon | Forall | Arrow | Dot
           | Let | Eq | In | Fix
           | TypeInt
           | If0 | Then | Else
           | Comma
           | Op J.Op
           | LowId String | UpId String
           | Int Integer
           | TupleField Int
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
