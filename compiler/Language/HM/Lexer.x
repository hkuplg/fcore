{
module Language.HM.Lexer 
    ( lexer
    , Token(..)
    ) where

import Language.HM.Syntax
}

%wrapper "basic"

$alpha = [A-Za-z]
$digit = [0-9]

tokens :-

    $white+     ;
    "#".*       ;
    "--".*      ;
    "//".*      ;

    \(          { \_ -> TkOParen }
    \)          { \_ -> TkCParen }
    \\          { \_ -> TkLam }
    \.          { \_ -> TkDot }
    \-\>        { \_ -> TkArrow }
    \:          { \_ -> TkColon }
    forall      { \_ -> TkForall }
    Int         { \_ -> TkTyInt }
    Bool        { \_ -> TkTyBool }
    let         { \_ -> TkLet }
    rec         { \_ -> TkRec }
    \=          { \_ -> TkEq }
    and         { \_ -> TkAnd }
    in          { \_ -> TkIn }
    if          { \_ -> TkIf }
    then        { \_ -> TkThen }
    else        { \_ -> TkElse }

    \!          { \_ -> TkUnOp Not }

    \*          { \_ -> TkBinOp Mul   }
    \/          { \_ -> TkBinOp Div    }
    \%          { \_ -> TkBinOp Mod    }
    \+          { \_ -> TkBinOp Add    }
    \-          { \_ -> TkBinOp Sub    }
    \<          { \_ -> TkBinOp Lt  }
    \>          { \_ -> TkBinOp Gt  }
    \<\=        { \_ -> TkBinOp Le }
    \>\=        { \_ -> TkBinOp Ge }
    \=\=        { \_ -> TkBinOp Eq  }
    \!\=        { \_ -> TkBinOp Ne  }
    \&\&        { \_ -> TkBinOp And   }
    \|\|        { \_ -> TkBinOp Or    }

    [A-Z] [$alpha $digit \_ \']*  { \s -> TkUpperId s }
    [a-z] [$alpha $digit \_ \']*  { \s -> TkLowerId s }
    \_ $digit+                    { \s -> TkUnderId (read (tail s))  }

    $digit+     { \s -> TkInteger (read s) }
    True        { \s -> TkBool True }
    False       { \s -> TkBool False }

{
data Token = TkOParen | TkCParen
           | TkLam | TkDot | TkArrow | TkColon
           | TkLet | TkRec | TkEq | TkAnd | TkIn
           | TkIf | TkThen | TkElse
           | TkForall | TkTyInt | TkTyBool
           | TkComma
           | TkBinOp BinOp | TkUnOp UnOp
           | TkUpperId String | TkLowerId String | TkUnderId Int
           | TkInteger Integer | TkBool Bool
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
