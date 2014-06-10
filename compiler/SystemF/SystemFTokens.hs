module SystemFTokens where

import Language.Java.Syntax as J

data SystemFToken = TokenTLambda
                  | TokenLambda
                  | TokenFix
                  | TokenComma
                  | TokenDot
                  | TokenArrow
                  | TokenColon
                  | TokenLet
                  | TokenEQ
                  | TokenIn
                  | TokenOParen
                  | TokenCParen
                  | TokenForall
                  | TokenIntType
                  | TokenLowId String
                  | TokenUpId String
                  | TokenInt Integer
                  | TokenIf0
                  | TokenThen
                  | TokenElse
                  | TokenTupleField Int
                  | TokenPrimOp J.Op
                  deriving (Eq, Show)
