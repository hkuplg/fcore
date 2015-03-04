{
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- |
Module      :  Lexer
Description :  Lexer for F2J
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable

References:
[1] https://www.haskell.org/happy/doc/happy.pdf
[2] https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x
-}

module Lexer
    ( lexer
    , Token(..)
    , Located(..)
    , getLocation
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

    \(          { locate (\_ _ -> Toparen) }
    \)          { locate (\_ _ -> Tcparen) }
    \[          { locate (\_ _ -> Tobrack) }
    \]          { locate (\_ _ -> Tcbrack) }
    \::         { locate (\_ _ -> Tdcolon) }
    \{          { locate (\_ _ -> Tocurly) }
    \}          { locate (\_ _ -> Tccurly) }
    \/\\        { locate (\_ _ -> Ttlam) }
    \\          { locate (\_ _ -> Tlam) }
    \:          { locate (\_ _ -> Tcolon) }
    \;          { locate (\_ _ -> Tsemi) }
    forall      { locate (\_ _ -> Tforall) }
    \-\>        { locate (\_ _ -> Tarrow) }
    \.          { locate (\_ _ -> Tdot) }
    \&          { locate (\_ _ -> Tandtype) }
    \,\,        { locate (\_ _ -> Tmerge) }
    with        { locate (\_ _ -> Twith) }
    \'          { locate (\_ _ -> Tquote) }
    -- this     { locate (\_ _ -> Tthis) }
    -- super    { locate (\_ _ -> Tsuper) }
    type        { locate (\_ _ -> Ttype) }
    let         { locate (\_ _ -> Tlet) }
    rec         { locate (\_ _ -> Trec) }
    \=          { locate (\_ _ -> Teq) }
    and         { locate (\_ _ -> Tand) }
    Int         { locate (\_ _ -> Tjavaclass "java.lang.Integer") }
    String      { locate (\_ _ -> Tjavaclass "java.lang.String") }
    Bool        { locate (\_ _ -> Tjavaclass "java.lang.Boolean") }
    Char        { locate (\_ _ -> Tjavaclass "java.lang.Character") }
    Float       { locate (\_ _ -> Tjavaclass "java.lang.Float") }
    Double      { locate (\_ _ -> Tjavaclass "java.lang.Double") }
    List        { locate (\_ _ -> Tlist) }
    Tree        { locate (\_ _ -> Tjavaclass "f2j.FunctionalTree") }
    if          { locate (\_ _ -> Tif) }
    then        { locate (\_ _ -> Tthen) }
    else        { locate (\_ _ -> Telse) }
    \,          { locate (\_ _ -> Tcomma) }
    new         { locate (\_ _ -> Tnew) }
    module      { locate (\_ _ -> Tmodule) }
    data        { locate (\_ _ -> Tdata) }
    \|          { locate (\_ _ -> Tbar) }
    case        { locate (\_ _ -> Tcase) }
    of          { locate (\_ _ -> Tof) }

    -- Literals
    $digit+                { locate (\_ s -> Tint (read s)) }
    \"($printable # \")*\" { locate (\_ s -> Tstring (init $ tail s)) }
    \'($printable # \')\'  { locate (\_ s -> Tchar (s !! 1)) }
    True                   { locate (\_ s -> Tbool True) }
    False                  { locate (\_ s -> Tbool False) }
    Empty                  { locate (\_ _ -> Temptytree) }
    Fork                   { locate (\_ _ -> Tnonemptytree ) }
    head                   { locate (\_ _ -> Tlisthead) }
    tail                   { locate (\_ _ -> Tlisttail) }
    cons                   { locate (\_ _ -> Tlistcons) }
    isNil                  { locate (\_ _ -> Tlistisnil) }
    length                 { locate (\_ _ -> Tlistlength) }
    \(\)                   { locate (\_ _ -> Tunitlit ) }
    Unit                   { locate (\_ _ -> Tunit) }

    -- java.package.path.Classname
    ([a-z] [$vchar]* \.)+ [A-Z] [$vchar]*  { locate (\_ s -> Tjavaclass s) }

    -- ID
    [A-Z] [$vchar]*     { locate (\_ s -> Tupperid s) }
    [a-z] [$vchar]*     { locate (\_ s -> Tlowerid s) }
    \_ $digit+          { locate (\_ s -> Tunderid (read (tail s))) }

    -- http://hackage.haskell.org/package/language-java-0.2.5/docs/src/Language-Java-Syntax.html#Op
    \*          { locate (\_ _ -> Tprimop J.Mult   ) }
    \/          { locate (\_ _ -> Tprimop J.Div    ) }
    \%          { locate (\_ _ -> Tprimop J.Rem    ) }
    \+          { locate (\_ _ -> Tprimop J.Add    ) }
    \-          { locate (\_ _ -> Tprimop J.Sub    ) }
    \<          { locate (\_ _ -> Tprimop J.LThan  ) }
    \<\=        { locate (\_ _ -> Tprimop J.LThanE ) }
    \>          { locate (\_ _ -> Tprimop J.GThan  ) }
    \>\=        { locate (\_ _ -> Tprimop J.GThanE ) }
    \=\=        { locate (\_ _ -> Tprimop J.Equal  ) }
    \!\=        { locate (\_ _ -> Tprimop J.NotEq  ) }
    \&\&        { locate (\_ _ -> Tprimop J.CAnd   ) }
    \|\|        { locate (\_ _ -> Tprimop J.COr    ) }

{
data Token = Toparen | Tcparen | Tocurly | Tccurly
           | Ttlam | Tlam | Tcolon | Tforall | Tarrow | Tdot | Tandtype | Tmerge | Twith | Tquote
           -- | Tthis | Tsuper
           | Ttype | Tlet | Trec | Teq | Tand
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

data Located a = Located AlexPosn a

getLocation :: Located a -> (Int, Int)
getLocation (Located (AlexPn _ line col) _) = (line, col)

-- Modify a normal rule inside { ... } so that it returns a *located* token.
locate
  :: (AlexPosn -> String -> Token)
  -> (AlexPosn -> String -> Located Token)
locate f = \p s -> Located p (f p s)

lexer :: String -> [Located Token]
lexer = alexScanTokens
}
