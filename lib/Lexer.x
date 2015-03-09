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
[3] https://hackage.haskell.org/package/language-java
-}

module Lexer
    ( lexer
    , Token(..)
    , Located(..)
    , getLocation
    ) where

import qualified Language.Java.Syntax as J (Op(..))
import Numeric (readOct)
import Data.Char (isHexDigit, isOctDigit)

}

%wrapper "posn"

$alpha = [A-Za-z]
$digit = [0-9]

$vchar = [$alpha $digit \_ \']

-- Use Java string specification
-- From Language.Java v0.2.7 (BSD3 License)
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
@octEscape  = [0123]? $octdig{1,2}
@hexEscape  = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])

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
    \"(@charEscape | $printable # [\" \\])*\" { locate (\_ s -> Tstring (convChar . init . tail $ s)) }
    \'(@charEscape | $printable # [\' \\])\'  { locate (\_ s -> Tchar (head . convChar . init .tail $ s)) }
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
           | Tmodule
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

-- From Language.Java v0.2.7 (BSD3 License)
lexicalError :: String -> a
lexicalError = error . ("lexical error: " ++)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> String
convChar ('\\':'u':s@(d1:d2:d3:d4:s')) =
  -- TODO: this is the wrong place for handling unicode escapes
  -- according to the Java Language Specification. Unicode escapes can
  -- appear anywhere in the source text, and are best processed
  -- before lexing.
  if all isHexDigit [d1,d2,d3,d4]
  then toEnum (read ['0','x',d1,d2,d3,d4]):convChar s'
  else lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':'u':s) =
  lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':c:s) =
  if isOctDigit c
  then convOctal maxRemainingOctals
  else (case c of
          'b' -> '\b'
          'f' -> '\f'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          '\'' -> '\''
          '\\' -> '\\'
          '"' -> '"'
          _ -> badEscape):convChar s
  where maxRemainingOctals =
          if c <= '3' then 2 else 1
        convOctal n =
          let octals = takeWhile isOctDigit $ take n s
              noctals = length octals
              toChar = toEnum . fst . head . readOct
          in toChar (c:octals):convChar (drop noctals s)
        badEscape = lexicalError $ "bad escape \"\\" ++ c:"\""
convChar ("\\") =
  lexicalError "bad escape \"\\\""
convChar (x:s) = x:convChar s
convChar "" = ""

lexer :: String -> [Located Token]
lexer = alexScanTokens
}
