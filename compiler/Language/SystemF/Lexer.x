{
module Language.SystemF.Lexer (lexer, Token (..)) where

import qualified Language.Java.Syntax as J (Op (..))
}

%wrapper "basic"

$alpha = [A-Za-z]
$digit = [0-9]

tokens :-

    $white+     ;
    "#".*       ;
    "--".*      ;
    "//".*      ;

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
    \*          { \_ -> PrimOp J.Mult   }
    \/          { \_ -> PrimOp J.Div    }
    \%          { \_ -> PrimOp J.Rem    }
    \+          { \_ -> PrimOp J.Add    }
    \-          { \_ -> PrimOp J.Sub    }
    \<          { \_ -> PrimOp J.LThan  }
    \>          { \_ -> PrimOp J.GThan  }
    \<\=        { \_ -> PrimOp J.LThanE }
    \>\=        { \_ -> PrimOp J.GThanE }
    \=\=        { \_ -> PrimOp J.Equal  }
    \!\=        { \_ -> PrimOp J.NotEq  }
    \&\&        { \_ -> PrimOp J.CAnd   }
    \|\|        { \_ -> PrimOp J.COr    }

    [A-Z] [$alpha $digit \_ \']*  { \s -> UpperId s }
    [a-z] [$alpha $digit \_ \']*  { \s -> LowerId s }
    \_ $digit+                    { \s -> UnderId (read (tail s))  }

    $digit+    { \s -> Integer (read s) }

{

data Token = OParen | CParen
           | TLam | Lam | Colon | Forall | Arrow | Dot
           | Let | Eq | In | Fix
           | TypeInt
           | If0 | Then | Else
           | Comma
           | PrimOp J.Op 
           | UpperId String | LowerId String | UnderId Int
           | Integer Integer
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
