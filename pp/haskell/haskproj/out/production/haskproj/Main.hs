module Main where
import Lib


--- Lecture 3

-- ex 2

data RecType = RecInt | RecBool | Arrow RecType RecType | Prod RecType RecType deriving Show

-- ex 3

type Var = String
data Expr = X Var | C Const | Paren Expr Expr | Pair Expr Expr | If Expr Expr Expr |
            Lambda Var Expr | Let Var Expr Expr
data Const = Number | Plus | Equal | IsZero | True | False | Not | First | Second

-- ex 4

data LookupT x y lst = Nothing | Lookup x y
--lookup (x,y) lst = []



main = print "done"
