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
                  | TokenTVar String
                  deriving (Eq, Show)
