module Expr where

data Expr = Poly Pol | Add Expr Expr | Mult Expr Expr | Div Expr Expr | Diff Expr | ExprPow Expr Pow | Sin Expr | Cos Expr deriving (Show, Eq)
data Pol = Var Id | Con Const | PolScale Const Pol | PolPow Id Pow | Parents Expr deriving (Show, Eq)

type Id = String
type Const = Int
type Pow = Int

prettyPrint expr = case expr of
    (Poly pol) -> prettyPrintPol pol
    (Add e1 e2) -> prettyPrint e1 ++ " + " ++ prettyPrint e2
    (Mult e1 e2) -> prettyPrint e1 ++ " * " ++ prettyPrint e2
    (Div e1 e2) -> prettyPrint e1 ++ " / " ++ prettyPrint e2
    (Diff e1) -> "dx(" ++ prettyPrint e1 ++ ")"
    (ExprPow e1 pow) -> prettyPrint e1 ++ " ^ " ++ show pow
    (Sin e1) -> "sin(" ++ prettyPrint e1 ++ ")"
    (Cos e1) -> "cos(" ++ prettyPrint e1 ++ ")"

prettyPrintPol pol = case pol of
    (Var id) -> id
    (Con c) -> show c
    (PolScale c p) -> show c ++ " * " ++ prettyPrintPol p
    (PolPow id pow) -> id ++ " ^ " ++ show pow
    (Parents e1) -> "(" ++ prettyPrint e1 ++ ")"


--reduce function. Takes an expression (which is in tree form) and reduces recursively untill no changes are happening anymore
reduce :: Expr -> Expr

reduce (Add (ExprPow (Sin (Poly (Var x1))) 2) (ExprPow (Cos (Poly (Var x2))) 2)) = Poly (Con 1)
reduce (Add e1 e2) = Add (reduce e1) (reduce e2)

reduce (Mult (Poly (PolPow var1 ex1)) (Poly (PolPow var2 ex2))) = Poly (PolPow var1 (ex1 + ex2))
reduce (Mult (Poly (Var e1)) (Poly (Var e2))) = if e1==e2 then Poly (PolPow e1 2) else Mult (Poly (Var e1)) (Poly (Var e2))
reduce (Mult (Poly (Var e1)) (Poly (Parents (Add (Poly (Var e2)) (Poly (Var e3)))))) =
       Add (Mult (Poly (Var e1)) (Poly (Var e2))) (Mult (Poly (Var e1)) (Poly (Var e3)))
reduce (Mult e1 e2) = Mult (reduce e1) (reduce e2)

reduce (Div (Poly (PolPow var1 ex1)) (Poly (PolPow var2 ex2))) = Poly (PolPow var1 (ex1 - ex2))
reduce (Div e1 e2) = Div (reduce e1) (reduce e2)

reduce (Diff e) = reduce (derive e)

reduce (ExprPow e1 pow) = ExprPow (reduce e1) pow

reduce (Poly (PolScale 1 (Var id))) = Poly (Var id)
reduce (Poly (PolScale 1 (PolPow id pow))) = Poly (PolPow id pow)
reduce (Poly (PolScale c (PolPow x 1))) = Poly (PolScale c (Var x))

reduce (Sin e) = Sin (reduce e)
reduce (Cos e) = Cos (reduce e)

reduce (Poly (PolScale c (Parents e))) = Poly (PolScale c (Parents (reduce e)))
reduce (Poly (PolPow id pow)) = Poly (PolPow id pow)
reduce (Poly (Parents (Poly (Var id)))) = Poly (Var id)
reduce (Poly (Parents e)) = Poly (Parents (reduce e))
reduce (Poly pol) = Poly pol


-- diff
derive :: Expr -> Expr
derive (Poly (PolPow x pow)) = Poly (PolScale pow (PolPow x (pow-1)))
derive (Poly (PolScale c (Var x))) = Poly (Con c)
derive (Poly (Var x)) = Poly (Con 1)
derive (Add e1 e2) = Add (derive e1) (derive e2)
derive (Sin e) = Cos e
derive (Cos e) = Sin e
derive (Diff e) = derive e






-- dx(x^4 + dx(cos (x)))






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