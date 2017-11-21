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
  $alpha+                       { \s -> TokenSym s }
  [\+]                          { \s -> TokenAdd }
  [\*]                          { \s -> TokenMul }
  [\\]                          { \s -> TokenDiv }
  [\^]                          { \s -> TokenPow }
  [\%]                          { \s -> TokenDiff }

{

data Token = TokenSin
           | TokenCos
           | TokenNum Int
           | TokenSym String
           | TokenAdd
           | TokenMul
           | TokenDiv
           | TokenPow
           | TokenDiff
             deriving (Eq,Show)

scanTokens = alexScanTokens

}