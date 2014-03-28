module HMTokens where

import HM

data HMToken = TokenLet | TokenRec | TokenAnd | TokenEQ | TokenIn
             | TokenLambda | TokenArrow
             | TokenOParen | TokenCParen
             | TokenBin BinOp | TokenComp CompOp
             | TokenIf | TokenThen | TokenElse
             | TokenInt Int
             | TokenLowId String | TokenUpId  String
             deriving (Eq, Show)
