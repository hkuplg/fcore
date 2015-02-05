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

-- From the Alex documentation (https://www.haskell.org/alex/doc/html/alex-files.html):
-- (1) When the input stream matches more than one rule, the rule which matches
--     the longest prefix of the input stream wins.
-- (2) If there are still several rules which match an equal number of
--     characters, then the rule which appears earliest in the file wins.

tokens :-

    $white+     ;
    "#".*       ;
    "--".*      ;
    "//".*      ;


    -- Keywords that lead declarations
    let         { \_ _ -> Tlet }
    module      { \_ _ -> Tmodule }
    type        { \_ _ -> Ttype }

    -- Other keywords
    and         { \_ _ -> Tand }
    case        { \_ _ -> Tcase }
    data        { \_ _ -> Tdata }
    else        { \_ _ -> Telse }
    end         { \_ _ -> Tend }
    forall      { \_ _ -> Tforall }
    if          { \_ _ -> Tif }
    in          { \_ _ -> Tin }
    new         { \_ _ -> Tnew }
    of          { \_ _ -> Tof }
    rec         { \_ _ -> Trec }
    then        { \_ _ -> Tthen }
    with        { \_ _ -> Twith }
    -- this        { \_ _ -> Tthis }
    -- super       { \_ _ -> Tsuper }


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
    \-\>        { \_ _ -> Tarrow }
    \.          { \_ _ -> Tdot }
    \&          { \_ _ -> Tandtype }
    \,\,        { \_ _ -> Tmerge }
    \'          { \_ _ -> Tquote }
    \=          { \_ _ -> Teq }
    Int         { \_ _ -> Tjavaclass "java.lang.Integer" }
    String      { \_ _ -> Tjavaclass "java.lang.String" }
    Bool        { \_ _ -> Tjavaclass "java.lang.Boolean" }
    Char        { \_ _ -> Tjavaclass "java.lang.Character" }
    Double      { \_ _ -> Tjavaclass "java.lang.Double" }
    List        { \_ _ -> Tlist}
    Tree        { \_ _ -> Tjavaclass "f2j.FunctionalTree" }
    \,          { \_ _ -> Tcomma }
    \|          { \_ _ -> Tbar }

    -- Literals
    $digit+                { \_ s -> Tint (read s) }
    \"($printable # \")*\" { \_ s -> Tstring (init $ tail s) }
    \'($printable # \')\'  { \_ s -> Tchar (s !! 1) }
    True                   { \_ s -> Tbool True}
    False                  { \_ s -> Tbool False}
    Empty                  { \_ _ -> Temptytree}
    Fork                   { \_ _ -> Tnonemptytree }
    head                   { \_ _ -> Tlisthead}
    tail                   { \_ _ -> Tlisttail}
    cons                   { \_ _ -> Tlistcons}
    isNil                  { \_ _ -> Tlistisnil}
    length                 { \_ _ -> Tlistlength}
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
           | Tlist | Tlisthead | Tlisttail | Tlistcons | Tlistisnil | Tlistlength
           | Tdata | Tcase | Tbar | Tof | Tto
           deriving (Eq, Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}
