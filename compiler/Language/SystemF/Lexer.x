{
module Language.SystemF.Lexer
    ( lexer
    , Token (..)
    ) where

import qualified Language.Java.Syntax as J (Op(..))
}

%wrapper "posn"

$alpha = [A-Za-z]
$digit = [0-9]

tokens :-

    $white+     ;
    "#".*       ;
    "--".*      ;
    "//".*      ;

    \(          { \_ _ -> OParen }
    \)          { \_ _ -> CParen }
    \/\\        { \_ _ -> TLam }
    \\          { \_ _ -> Lam }
    \:          { \_ _ -> Colon }
    forall      { \_ _ -> Forall }
    \-\>        { \_ _ -> Arrow }
    \.          { \_ _ -> Dot }
    let         { \_ _ -> Let }
    rec         { \_ _ -> Rec }
    \=          { \_ _ -> Eq }
    and         { \_ _ -> And }
    in          { \_ _ -> In }
    fix         { \_ _ -> Fix }
    Int         { \_ _ -> Int }
    if0         { \_ _ -> If0 }
    then        { \_ _ -> Then }
    else        { \_ _ -> Else }
    \,          { \_ _ -> Comma }

    [A-Z] [$alpha $digit \_ \']*  { \_ s -> UpperId s }
    [a-z] [$alpha $digit \_ \']*  { \_ s -> LowerId s }
    \_ $digit+                    { \_ s -> UnderId (read (tail s))  }

    $digit+     { \_ s -> Integer (read s) }

    -- http://hackage.haskell.org/package/language-java-0.2.5/docs/src/Language-Java-Syntax.html#Op
    \*          { \_ _ -> PrimOp J.Mult   }
    \/          { \_ _ -> PrimOp J.Div    }
    \%          { \_ _ -> PrimOp J.Rem    }
    \+          { \_ _ -> PrimOp J.Add    }
    \-          { \_ _ -> PrimOp J.Sub    }
    \<          { \_ _ -> PrimOp J.LThan  }
    \<\=        { \_ _ -> PrimOp J.LThanE }
    \>          { \_ _ -> PrimOp J.GThan  }
    \>\=        { \_ _ -> PrimOp J.GThanE }
    \=\=        { \_ _ -> PrimOp J.Equal  }
    \!\=        { \_ _ -> PrimOp J.NotEq  }
    \&\&        { \_ _ -> PrimOp J.CAnd   }
    \|\|        { \_ _ -> PrimOp J.COr    }

{
data Token = OParen | CParen
           | TLam | Lam | Colon | Forall | Arrow | Dot
           | Let | Rec | Eq | And | In | Fix
           | Int
           | If0 | Then | Else
           | Comma
           | UpperId String | LowerId String | UnderId Int
           | Integer Integer
           | PrimOp J.Op
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
