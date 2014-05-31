module SystemFTokens where

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
                  deriving (Eq, Show)
