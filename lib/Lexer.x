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
    , Alex(..)
    , alexError
    , runAlex
    ) where

import qualified Language.Java.Syntax as J (Op(..))
import Data.Char (isOctDigit, chr)
import SrcLoc

}

%wrapper "monad"

$alpha = [A-Za-z]
$digit = [0-9]

$vchar = [$alpha $digit \_ \']

$symbols = [\! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~]

-- Use Java string specification
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
@octEscape  = [0123]? $octdig{1,2}
@hexEscape  = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])

tokens :-

    <0, strexp> $white+ ;
    <0> "--".*      ;
    <0> "{-"        { nested_comment }
    <0> \"          { start_string `andBegin` str }
    <str> (@charEscape | $printable # [\" \\] | \\\{) { save_string }
    <strexp> \}     { end_strexp `andBegin` str }
    <str> \"        { end_string `andBegin` 0 }

    <0> \}          { locate (\_ _ -> Tccurly) }
    <0, strexp>     {

    \(          { locate (\_ _ -> Toparen) }
    \)          { locate (\_ _ -> Tcparen) }
    \[          { locate (\_ _ -> Tobrack) }
    \]          { locate (\_ _ -> Tcbrack) }
    \::         { locate (\_ _ -> Tdcolon) }
    \{          { locate (\_ _ -> Tocurly) }
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
    in          { locate (\_ _ -> Tin) }
    \=          { locate (\_ _ -> Teq) }
    and         { locate (\_ _ -> Tand) }
    Int         { locate (\_ _ -> Tjavaclass "java.lang.Integer") }
    String      { locate (\_ _ -> Tjavaclass "java.lang.String") }
    Bool        { locate (\_ _ -> Tjavaclass "java.lang.Boolean") }
    Char        { locate (\_ _ -> Tjavaclass "java.lang.Character") }
    Float       { locate (\_ _ -> Tjavaclass "java.lang.Float") }
    Double      { locate (\_ _ -> Tjavaclass "java.lang.Double") }
    Tree        { locate (\_ _ -> Tjavaclass "f2j.FunctionalTree") }
    if          { locate (\_ _ -> Tif) }
    then        { locate (\_ _ -> Tthen) }
    else        { locate (\_ _ -> Telse) }
    \,          { locate (\_ _ -> Tcomma) }
    new         { locate (\_ _ -> Tnew) }
    module      { locate (\_ _ -> Tmodule) }
    import      { locate (\_ _ -> Timport) }
    package     { locate (\_ _ -> TPackage) }
    data        { locate (\_ _ -> Tdata) }
    \|          { locate (\_ _ -> Tbar) }
    case        { locate (\_ _ -> Tcase) }
    of          { locate (\_ _ -> Tof) }
    \_          { locate (\_ _ -> Tunderscore) }
    \`          { locate (\_ _ -> Tbackquote) }
    error       { locate (\_ _ -> Terror)}

    -- Literals
    $digit+                { locate (\_ s -> Tint (read s)) }
    \'(@charEscape | $printable # [\' \\])\'  { convChar False }
    True                   { locate (\_ s -> Tbool True) }
    False                  { locate (\_ s -> Tbool False) }
    Empty                  { locate (\_ _ -> Temptytree) }
    Fork                   { locate (\_ _ -> Tnonemptytree ) }
    \(\)                   { locate (\_ _ -> Tunitlit ) }
    Unit                   { locate (\_ _ -> Tunit) }
    L\[                    { locate (\_ _ -> Tlistbegin)}

    -- java.package.path.Classname
    ([a-z] [$vchar]* \.)+ [A-Z] [$vchar]*  { locate (\_ s -> Tjavaclass s) }

    -- ID
    [A-Z] [$vchar]*                                      { locate (\_ s -> Tupperid s) }
    \_ [$alpha \_] [$vchar]* | [a-z] [$vchar]*           { locate (\_ s -> Tlowerid s) }
    \_ $digit+                                           { locate (\_ s -> Tunderid (read (tail s))) }

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

    $symbols+   { locate (\_ s -> Tsymbolid s) }

    } -- end of start code scope

{
data Token = Toparen | Tcparen | Tocurly | Tccurly
           | Ttlam | Tlam | Tcolon | Tforall | Tarrow | Tdot | Tandtype | Tmerge | Twith | Tquote | Tbackquote
           -- | Tthis | Tsuper
           | Ttype | Tlet | Trec | Tin | Teq | Tand
           | Tjavaclass String
           | Tnew
           | Tif | Tthen | Telse
           | Tcomma | Tsemi
           | Tupperid String | Tlowerid String | Tunderid Int | Tsymbolid String
           | Tint Integer | Tbool Bool | Tchar Char | Tunitlit | Tunit
           | Tprimop J.Op
           | Tobrack | Tcbrack | Tdcolon
           | Tmodule | Timport | TPackage
           | Temptytree | Tnonemptytree
           | Tlistbegin
           | Tdata | Tcase | Tbar | Tof | Tto | Tunderscore
           | Teof | Tschar Char | Tstrl | Tstrr | Tstrexpl | Tstrexpr
           | Terror
           deriving (Eq, Show)

-- Modify a normal rule inside { ... } so that it returns a *located* token.
locate
  :: (AlexPosn -> String -> Token)
  -> (AlexInput -> Int -> Alex (Located Token))
locate f = \(p@(AlexPn _ l c),_,_,s) len -> return (L (Loc l c) (f p (take len s)))

-- Monadic lexer
-- https://github.com/simonmar/alex/blob/master/examples/haskell.x
alexEOF :: Alex (Located Token)
alexEOF = do
  ((AlexPn _ l c),_,_,_) <- alexGetInput
  return (L (Loc l c) Teof)

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

lexError :: String -> Alex a
lexError s = do
  (p,c,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
                   (if (not (null input))
                     then " before " ++ show (head input)
                     else " at end of file"))

-- Nested comment block
nested_comment :: AlexInput -> Int -> Alex (Located Token)
nested_comment _ _ = do
  input <- alexGetInput
  go 1 input
  where go 0 input = do alexSetInput input; alexMonadScan
        go n input = do
          case alexGetByte input of
            Nothing  -> err input
            Just (c,input) -> do
              case chr (fromIntegral c) of
                '-' -> do
                  case alexGetByte input of
                    Nothing  -> err input
                    Just (125,input) -> go (n-1) input
                    Just (c,input)   -> go n input
                '\123' -> do
                  case alexGetByte input of
                    Nothing  -> err input
                    Just (c,input) | c == fromIntegral (ord '-') -> go (n+1) input
                    Just (c,input)   -> go n input
                c -> go n input
        err input = do alexSetInput input; lexError "error in nested comment"

-- String interpolation
start_string ((AlexPn _ l c),_,_,_) _ = return $ L (Loc l c) Tstrl
end_string ((AlexPn _ l c),_,_,_) _ = return $ L (Loc l c) Tstrr
start_strexp ((AlexPn _ l c),_,_,_) _ = return $ L (Loc l c) Tstrexpl
end_strexp ((AlexPn _ l c),_,_,_) _ = return $ L (Loc l c) Tstrexpr
save_string p@(_,_,_,s) len = do
  let action = if take len s == "\\{" then start_strexp `andBegin` strexp
               else convChar True
  action p len

-- Character escape
unescape :: String -> (Char, Bool)
unescape [] = ('\0', True)
unescape ('\\':'u':s) = (read ("'\\x" ++ s ++ "'"), True)
unescape ('\\':s) | all isOctDigit s && length s <= 3 = (read ("'\\o" ++ s ++ "'"), True)
                  | length s == 1 = (read ("'\\" ++ s ++ "'"), True)
                  | otherwise = ('\0', False)
unescape (x:_) = (x, True)

convChar :: Bool -> AlexInput -> Int -> Alex (Located Token)
convChar isSChar (p@(AlexPn _ l c),_,_,s) len = do
  let s' = take len s
      raw = if isSChar then s' else init (tail s')
      (ch, ret) = unescape raw
  if ret then
    if isSChar then return (L (Loc l c) (Tschar ch))
    else return (L (Loc l c) (Tchar ch))
  else alexError (showPosn p ++ ": bad escape \"" ++ s ++ "\"")

lexer :: (Located Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)
}
