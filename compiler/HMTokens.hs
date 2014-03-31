module HMTokens where

import HMSyntax

data HMToken = TokenLet | TokenRec | TokenAnd | TokenEQ | TokenIn
             | TokenLambda | TokenArrow
             | TokenOParen | TokenCParen
             | TokenUn UnOp | TokenBin BinOp 
             | TokenIf | TokenThen | TokenElse
             | TokenInt Int
             | TokenLowId String | TokenUpId  String
             deriving (Eq, Show)
