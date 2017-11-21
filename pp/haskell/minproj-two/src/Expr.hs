module Expr where

data Expr = Poly Pol | Add Expr Expr | Mult Expr Expr | Div Expr Expr | Diff Expr | ExprPow Expr Pow | Sin Expr | Cos Expr deriving (Show, Eq)
data Pol = Var Id | Con Const | PolScale Pol Pol | PolAdd Pol Pol | PolPow Id Pow | Parents Expr deriving (Show, Eq)

type Id = String
type Const = Int
type Pow = Int

source :: Expr -> String
source expr = "Not Implemented"

--type Id = String
--data Op = Add | Sub | Mul deriving (Eq,Show)
--data Expr = Abs Id Expr
--          | App Expr Expr
--          | Var Id
--          | Num Int
--          | Binop Op Expr Expr
--          deriving (Eq,Show)

--data Expr = Poly Pol | Add Expr Expr | Mult Expr Expr | Div Expr Expr | Diff Id Expr | Sin Expr | Cos Expr deriving (Show, Eq)
--data Pol = Var Id | PolScale Const Pol | PolAdd Pol Pol | PolPow Id Pow deriving (Show, Eq)

--source expr = case expr of
--  (Abs x e) -> parens $ "\\" ++ x ++ " -> " ++ source e
--  (App e1 e2) -> parens $ source e1 ++ " " ++ source e2
--  (Binop op e1 e2) -> parens $ source e1 ++ sourceOp op ++ source e2
--  (Var x) -> x
--  (Num n) -> show n
--  where sourceOp Add = " + "
--        sourceOp Sub = " - "
--        sourceOp Mul = " * "
--        parens s = "(" ++ s ++ ")"

--addExpr :: Expr -> Expr -> Expr
--addExpr = Binop Add
--
--subExpr :: Expr -> Expr -> Expr
--subExpr = Binop Sub
--
--mulExpr :: Expr -> Expr -> Expr
--mulExpr = Binop Mul