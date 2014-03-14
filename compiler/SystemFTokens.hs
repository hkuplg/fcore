module SystemFTokens where

data SystemFToken = TokenTLambda
                  | TokenLambda
                  | TokenDot
                  | TokenArrow
                  | TokenColon
                  | TokenLet
                  | TokenEQ
                  | TokenIn
                  | TokenOParen
                  | TokenCParen
                  | TokenForall
                  | TokenInt
                  | TokenLowId String
                  | TokenUpId String
                  deriving (Eq, Show)
