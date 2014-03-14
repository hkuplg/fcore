module SystemFTokens where

data SystemFToken = TokenTLambda
                  | TokenLambda
                  | TokenDot
                  | TokenArrow
                  | TokenColon
                  | TokenOParen
                  | TokenCParen
                  | TokenForall
                  | TokenInt
                  | TokenLowId String
                  | TokenUpId String
                  deriving (Eq, Show)
