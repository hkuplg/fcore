{-
The uniform container (with its pretty printer) for holding all kinds of problems,
including, but not limited to SyntaxErrors and TypeErrors, in user programs.
-}
module Problem
  ( Problem(..)
  , ProblemLevel(..)
  , prettyProblems
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding (line, column)

data Problem = Problem
  { problemLevel    :: ProblemLevel
  , problemLocation :: (FilePath, Int, Int)
  , problemLine     :: String
  , problemDescription :: Doc
  }

data ProblemLevel = Error | Warning deriving (Eq, Show)

instance Pretty ProblemLevel where
  pretty Error   = dullred    (text "error")
  pretty Warning = dullyellow (text "warning")


prettyProblems :: [Problem] -> Doc
prettyProblems problems
  = vcat (map prettyProblem problems) <$$>
    int errorCount <+> text (pluralize "error" errorCount)
    <+> text "and" <+>
    int warningCount <+> text (pluralize "warning" warningCount)
    <+> text "found"
  where
    errorCount   = length $ filter (\p -> problemLevel p == Error) problems
    warningCount = length $ filter (\p -> problemLevel p == Warning) problems
    pluralize s n = if n > 1 then s ++ "s" else s

prettyProblem :: Problem -> Doc
prettyProblem problem
  = prettyLocation (problemLocation problem) <$$>
    problemLineWithCursor <$$>
    pretty (problemLevel problem) <> colon <+> problemDescription problem <> linebreak
  where
    problemLineWithCursor :: Doc
    problemLineWithCursor
      = text (problemLine problem) <$$>
        indent (let (_,_,column) = problemLocation problem in column - 1) (char '^')
    prettyLocation :: (FilePath, Int, Int) -> Doc
    prettyLocation (filePath, line, _column)
      = text "File" <+> dquotes (text filePath) <> comma <+> text "line" <+> int line

-- Test cases

testProblem1 :: Problem
testProblem1 = Problem
  { problemLevel    = Error
  , problemLocation = ("examples/foo.sf", 36, 15)
  , problemLine     = "        doc = Nokogiri::HTML(page.body, nil, @encoding)"
  , problemDescription = text "Undefined class: ‘Nokogiri’" }

testProblem2 :: Problem
testProblem2 = Problem
  { problemLevel    = Warning
  , problemLocation = ("problems.sf", 36, 22)
  , problemLine     = "    indent (let (_,_,column) = problemLocation problem in column - 1) (char '^')"
  , problemDescription = text "Undefined local variable or method `f' for main:Object (NameError)" }

main :: IO ()
main = print (prettyProblems [testProblem1, testProblem2])
