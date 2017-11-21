module Expr where

data Expr = Poly Pol | Add Expr Expr | Mult Expr Expr | Div Expr Expr | Diff Expr | ExprPow Expr Pow | Sin Expr | Cos Expr deriving (Show, Eq)
data Pol = Var Id | Con Const | PolScale Const Pol | PolPow Id Pow | Parents Expr deriving (Show, Eq)

type Id = String
type Const = Int
type Pow = Int


--reduce function. Takes an expression (which is in tree form) and reduces recursively untill no changes are happening anymore
reduce :: Expr -> Expr

-- reduction cases
reduce (Mult (Poly (PolPow var1 ex1)) (Poly (PolPow var2 ex2))) = Poly (PolPow var1 (ex1 + ex2))
reduce (Div (Poly (PolPow var1 ex1)) (Poly (PolPow var2 ex2))) = Poly (PolPow var1 (ex1 - ex2))
reduce (Add (ExprPow (Sin (Poly (Var "x"))) 2) (ExprPow (Cos (Poly (Var "x"))) 2)) = Poly (Con 1)
reduce (Add (Poly (Var e1)) (Poly (Var e2))) = Add (Poly (Var e2)) (Poly (Var e1))
reduce (Mult (Poly (Var e1)) (Poly (Var e2))) = Poly (PolPow e1 2)

-- general cases
reduce (Add e1 e2) = Add (reduce e1) (reduce e2)
reduce (Mult e1 e2) = Mult (reduce e1) (reduce e2)
reduce (Div e1 e2) = Div (reduce e1) (reduce e2)
reduce (Diff e) = reduce (derive e)
reduce (ExprPow e1 pow) = ExprPow (reduce e1) pow
reduce (Sin e) = Sin (reduce e)
reduce (Cos e) = Cos (reduce e)

-- Polynomial cases -- only PolScale and Parents can result in a reduction, the rest of the time we just return the polynomial
reduce (Poly (PolScale c (Parents e))) = reduce e
reduce (Poly (Parents e)) = reduce e
reduce (Poly pol) = Poly pol

-- Add (ExprPow (Sin (Poly (Var "x"))) 2) (ExprPow (Cos (Poly (Var "x"))) 2)

-- diff
derive :: Expr -> Expr
derive e = e











printExpr expr = show expr

--type Id = String
--data Op = Add | Sub | Mul deriving (Eq,Show)
--data Expr = Abs Id Expr
--          | App Expr Expr
--          | Var Id
--          | Num Int
--          | Binop Op Expr Expr
--          deriving (Eq,Show)

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