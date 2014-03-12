module SystemFTokens where

data SystemFToken = TokenTLambda
                  | TokenLambda
                  | TokenDot
                  | TokenArrow
                  | TokenColon
                  | TokenOParen
                  | TokenCParen
                  | TokenId String
                  deriving (Eq, Show)
