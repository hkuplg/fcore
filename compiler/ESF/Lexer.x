{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ESF.Lexer
    ( lexer
    , Token(..)
    ) where

import qualified Language.Java.Syntax as J (Op(..))

}

%wrapper "posn"

$alpha = [A-Za-z]
$digit = [0-9]

$vchar = [$alpha $digit \_ \']

tokens :-

    $white+     ;
    "#".*       ;
    "--".*      ;
    "//".*      ;

    \(          { \_ _ -> Toparen }
    \)          { \_ _ -> Tcparen }
    \{          { \_ _ -> Tocurly }
    \}          { \_ _ -> Tccurly }
    \/\\        { \_ _ -> Ttlam }
    \\          { \_ _ -> Tlam }
    \:          { \_ _ -> Tcolon }
    \;          { \_ _ -> Tsemi }
    forall      { \_ _ -> Tforall }
    \-\>        { \_ _ -> Tarrow }
    \.          { \_ _ -> Tdot }
    let         { \_ _ -> Tlet }
    rec         { \_ _ -> Trec }
    \=          { \_ _ -> Teq }
    and         { \_ _ -> Tand }
    in          { \_ _ -> Tin }
    Int         { \_ _ -> Tint }
    if          { \_ _ -> Tif }
    then        { \_ _ -> Tthen }
    else        { \_ _ -> Telse }
    \,          { \_ _ -> Tcomma }
    new         { \_ _ -> Tnew }

    -- Literal
    $digit+                { \_ s -> Tinteger (read s) }
    \"($printable # \")*\"  { \_ s -> Tstring (init $ tail s) }
    \'($printable # \')\'  { \_ s -> Tchar (s !! 1) }
    true                   { \_ s -> Tboolean True}
    false                  { \_ s -> Tboolean False}

    -- java.package.path.Classname
    ([a-z] [$vchar]* \.)+ [A-Z] [$vchar]*  { \_ s -> Tjavaclass s }

    -- ID
    [A-Z] [$vchar]*     { \_ s -> Tupperid s }
    [a-z] [$vchar]*     { \_ s -> Tlowerid s }
    \_ $digit+          { \_ s -> Tunderid (read (tail s))  }

    -- http://hackage.haskell.org/package/language-java-0.2.5/docs/src/Language-Java-Syntax.html#Op
    \*          { \_ _ -> Tprimop J.Mult   }
    \/          { \_ _ -> Tprimop J.Div    }
    \%          { \_ _ -> Tprimop J.Rem    }
    \+          { \_ _ -> Tprimop J.Add    }
    \-          { \_ _ -> Tprimop J.Sub    }
    \<          { \_ _ -> Tprimop J.LThan  }
    \<\=        { \_ _ -> Tprimop J.LThanE }
    \>          { \_ _ -> Tprimop J.GThan  }
    \>\=        { \_ _ -> Tprimop J.GThanE }
    \=\=        { \_ _ -> Tprimop J.Equal  }
    \!\=        { \_ _ -> Tprimop J.NotEq  }
    \&\&        { \_ _ -> Tprimop J.CAnd   }
    \|\|        { \_ _ -> Tprimop J.COr    }

{
data Token = Toparen | Tcparen | Tocurly | Tccurly
           | Ttlam | Tlam | Tcolon | Tforall | Tarrow | Tdot
           | Tlet | Trec | Teq | Tand | Tin
           | Tint | Tjavaclass String
           | Tnew
           | Tif | Tthen | Telse
           | Tcomma | Tsemi
           | Tupperid String | Tlowerid String | Tunderid Int
           | Tinteger Integer | Tstring String | Tboolean Bool | Tchar Char
           | Tprimop J.Op
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
