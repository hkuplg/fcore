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
                  | TokenId String
                  deriving (Eq, Show)
