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

type Vari = String

data LambdaType = Produ LambdaType | FUnc LambdaType LambdaType | Bools | Ints deriving(Eq,Show)

type Tenv = Var -> Maybe LambdaType

--tlookup [] x = Nothing
--tlookup ((x,t) : tenv1) y | x == y = Just t
--tlookup ((x,t) : tenv1) y | not (x == y) = tlookup tenv1 y

--remove [] x = []
--remove (h:tenv) x = if y == x then tenv else h : (remove tenv x)
--                        where (y,_) = h

-- tenvupdate tenv (x,t) = (x,t) : tenv1
--                       where tenv1 = remove tenv x
-- tenvupdate tenv (x,t) = \y -> if y == x then (Just t) else tenv y


main = print "done"
