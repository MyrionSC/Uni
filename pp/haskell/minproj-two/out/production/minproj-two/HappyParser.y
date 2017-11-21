{
module HappyParser where

import AlexToken
import Expr
}

%name expr
%tokentype { Token }
%error { parseError }

%token
    NUM   { TokenNum $$ }
    VAR   { TokenSym $$ }
    '+'   { TokenAdd }
    '*'   { TokenMul }
    '/'   { TokenDiv }
    '^'   { TokenPow }
    '%'   { TokenDiff }

%left '+'
%left '*'

%%

Expr : Expr '+' Expr               { Add $1 $3 }
     | Expr '*' Expr               { Mult $1 $3 }
     | Expr '/' Expr               { Div $1 $3 }
     | '%' Expr                    { Diff $2 }
     | Pol                         { Poly $1 }


Pol  : VAR '^' NUM                 { PolPow $1 $3 }
     | Var                         { $1 }

Var  : VAR                         { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}