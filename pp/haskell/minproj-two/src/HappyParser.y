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
    dx    { TokenDiff }
    sin   { TokenSin }
    cos   { TokenCos }
    '('   { TokenLParen }
    ')'   { TokenRParen }

%left '+'
%left '*' '/'
%left '^'
%left '%'
%left '(' ')'

%%

Expr : Expr '+' Expr               { Add $1 $3 }
     | Expr '*' Expr               { Mult $1 $3 }
     | Expr '/' Expr               { Div $1 $3 }
     | Expr '^' NUM                { ExprPow $1 $3 }
     | dx '(' Expr ')'             { Diff $3 }
     | sin '(' Expr ')'            { Sin $3 }
     | cos '(' Expr ')'            { Cos $3 }
     | Pol                         { Poly $1 }

Pol  : '(' Expr ')'                { Parents $2 }
     | VAR '^' NUM                 { PolPow $1 $3 }
     | NUM '*' Pol                 { PolScale $1 $3 }
     | Pol '+' Pol                 { PolAdd $1 $3 }
     | VAR                         { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Expr
parseExpr = expr . scanTokens
}