{
module SystemFParser where

import SystemFTokens
import SystemFJava
}

%name parseSystemF
%tokentype  { Token }
%error      { parseError }

%token

"/\\"  { TokenTLambda }
"\\"   { TokenLambda }
"->"   { TokenArrow }
"."    { TokenDot }
":"    { TokenColon }
"("    { TokenParenL }
")"    { TokenParenR }
id     { TokenId $$ }

%%

Program : id  { Nothing }

{
parseError :: [Token] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens
}
