{
{-# OPTIONS_GHC -w #-}
module AlexToken (Token(..),scanTokens) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;
  sin                           { \s -> TokenSin }
  cos                           { \s -> TokenCos }
  $digit+                       { \s -> TokenNum (read s) }
  $alpha [$alpha $digit]*       { \s -> TokenSym s }
  [\+]                          { \s -> TokenAdd }
  [\*]                          { \s -> TokenMul }
  [\/]                          { \s -> TokenDiv }
  [\^]                          { \s -> TokenPow }
  [\%]                          { \s -> TokenDiff }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }

{

data Token = TokenSin
           | TokenCos
           | TokenNum Int
           | TokenRat Rational
           | TokenSym String
           | TokenAdd
           | TokenMul
           | TokenDiv
           | TokenPow
           | TokenDiff
           | TokenLParen
           | TokenRParen
             deriving (Eq,Show)

scanTokens = alexScanTokens

}