{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lexer
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
    \[          { \_ _ -> Tobrack}
    \]          { \_ _ -> Tcbrack}
    \::         { \_ _ -> Tdcolon}
    \{          { \_ _ -> Tocurly }
    \}          { \_ _ -> Tccurly }
    \/\\        { \_ _ -> Ttlam }
    \\          { \_ _ -> Tlam }
    \:          { \_ _ -> Tcolon }
    \;          { \_ _ -> Tsemi }
    forall      { \_ _ -> Tforall }
    \-\>        { \_ _ -> Tarrow }
    \.          { \_ _ -> Tdot }
    \&          { \_ _ -> Tandtype }
    \,\,        { \_ _ -> Tmerge }
    with        { \_ _ -> Twith }
    \'          { \_ _ -> Tquote }
    -- this        { \_ _ -> Tthis }
    -- super       { \_ _ -> Tsuper }
    type        { \_ _ -> Ttype }
    let         { \_ _ -> Tlet }
    rec         { \_ _ -> Trec }
    \=          { \_ _ -> Teq }
    and         { \_ _ -> Tand }
    in          { \_ _ -> Tin }
    Int         { \_ _ -> Tjavaclass "java.lang.Integer" }
    String      { \_ _ -> Tjavaclass "java.lang.String" }
    Bool        { \_ _ -> Tjavaclass "java.lang.Boolean" }
    Char        { \_ _ -> Tjavaclass "java.lang.Character" }
    Double      { \_ _ -> Tjavaclass "java.lang.Double" }
    List        { \_ _ -> Tjavaclass "f2j.FunctionalList" }
    Tree        { \_ _ -> Tjavaclass "f2j.FunctionalTree" }
    if          { \_ _ -> Tif }
    then        { \_ _ -> Tthen }
    else        { \_ _ -> Telse }
    \,          { \_ _ -> Tcomma }
    new         { \_ _ -> Tnew }
    module      { \_ _ -> Tmodule }
    data        { \_ _ -> Tdata }
    \|          { \_ _ -> Tbar }
    case        { \_ _ -> Tcase }
    of          { \_ _ -> Tof }
    end         { \_ _ -> Tend }

    -- Literals
    $digit+                { \_ s -> Tint (read s) }
    \"($printable # \")*\" { \_ s -> Tstring (init $ tail s) }
    \'($printable # \')\'  { \_ s -> Tchar (s !! 1) }
    True                   { \_ s -> Tbool True}
    False                  { \_ s -> Tbool False}
    Empty                  { \_ _ -> Temptytree}
    Fork                   { \_ _ -> Tnonemptytree }
    \(\)                   { \_ _ -> Tunitlit }
    Unit                   { \_ _ -> Tunit }

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
           | Ttlam | Tlam | Tcolon | Tforall | Tarrow | Tdot | Tandtype | Tmerge | Twith | Tquote
           -- | Tthis | Tsuper
           | Ttype | Tlet | Trec | Teq | Tand | Tin
           | Tjavaclass String
           | Tnew
           | Tif | Tthen | Telse
           | Tcomma | Tsemi
           | Tupperid String | Tlowerid String | Tunderid Int
           | Tint Integer | Tstring String | Tbool Bool | Tchar Char | Tunitlit | Tunit
           | Tprimop J.Op
           | Tobrack | Tcbrack | Tdcolon
           | Tmodule | Tend
           | Temptytree | Tnonemptytree
	   | Tdata | Tcase | Tbar | Tof | Tto
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
