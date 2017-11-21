type Var = String       -- variable name
type Const = Rational   -- rational numbers
type Pow = Int          -- non-negative numbers

data Exp = Poly Polynomial | Add Exp Exp | Mult Exp Exp | Div Exp Exp | Diff Var Exp | Sin Exp | Cos Exp deriving (Show, Eq)
data Polynomial = Def Var | Scale Const Polynomial | AddPoly Polynomial Polynomial | PolyPower Var Pow deriving (Show, Eq)

--- --- Evaluation functions
--- Expression reducton function
reduce (Add (Mult (Sin var1) (Sin var2)) (Mult (Cos var3) (Cos var4))) = if var1==var2 && var1==var3 && var1==var4 then litteral 1 else (Add (Mult (Sin var1) (Sin var2)) (Mult (Cos var3) (Cos var4)))
reduce (Add expr1 expr2) = (Add expr1 expr2) -- base case

reduce (Mult expr (Poly (Scale 1 (PolyPower "litteral" 0)))) = expr
reduce (Mult (Poly (Scale 1 (PolyPower "litteral" 0))) expr) = expr
reduce (Mult (Poly (PolyPower var1 pow1)) (Poly (PolyPower var2 pow2))) = if var1 == var2 then (Poly (PolyPower var1 (pow1 + pow2))) else (Mult (power var1 pow1) (power var2 pow2))
reduce (Mult (Poly (PolyPower var1 pow1)) (Add (Poly (PolyPower var2 pow2)) (Poly (PolyPower var3 pow3)))) = (Add (Mult (Poly (PolyPower var1 pow1)) (Poly (PolyPower var2 pow2))) (Mult (Poly (PolyPower var1 pow1)) (Poly (PolyPower var3 pow3))))
reduce (Mult expr1 expr2) = Mult expr1 expr2 -- base case

reduce (Div (Poly (PolyPower var1 pow1)) (Poly (PolyPower var2 pow2))) = if var1 == var2 then (Poly (PolyPower var1 (pow1 - pow2))) else (Div (Poly (PolyPower var1 pow1)) (Poly (PolyPower var2 pow2)))
reduce (Div expr1 expr2) = (Div expr1 expr2) -- base case

reduce (Sin x) = (Sin x) -- base case
reduce (Cos x) = (Cos x) -- base case

reduce (Diff x (Add expr1 expr2)) = Add (derive expr1) (derive expr2)
reduce (Diff x (Poly (Scale con expr))) = (Mult (litteral con) (derive (Poly expr)))
reduce (Diff x (Mult expr1 expr2)) = Add (Mult (derive expr1) expr2) (Mult expr1 (derive expr2))
reduce (Diff x (Div expr1 expr2)) = Div (Add (Mult (derive expr1) expr2) (neg(Mult expr1 (derive expr2)))) (Mult expr2 expr2)

reduce (Poly polynomial) = Poly polynomial

reduce err = error(show("Reduce error case: ", err))

--- Recursive reduction function solve
solve expr = walk (reduce expr) expr

walk (Add expr1 expr2) prev = if (Add expr1 expr2)==prev then Add expr1 expr2 else  reduce(Add(walk (reduce expr1) (Add expr1 expr2)) (walk (reduce expr2) (Add expr1 expr2)))
walk (Mult expr1 expr2) prev = if (Mult expr1 expr2)==prev then Mult expr1 expr2 else reduce(Mult(walk (reduce expr1) (Mult expr1 expr2)) (walk(reduce expr2) (Mult expr1 expr2)))
walk (Div expr1 expr2) prev = if (Div expr1 expr2)==prev then Div expr1 expr2 else reduce(Div (walk (reduce expr2) (Div expr1 expr2)) (walk (reduce expr2) (Div expr1 expr2)))
walk (Sin expr) prev = if (Sin expr)==prev then Sin expr else reduce(Sin (walk (reduce expr) (Sin expr)))
walk (Cos expr) prev = if (Cos expr)==prev then Cos expr else reduce(Cos (walk (reduce expr) (Cos expr)))
walk (Diff x expr) prev = if (Diff x expr)==prev then Diff x expr else reduce(Diff x (walk (reduce expr) (Diff x expr)))

walk (Poly polynomial) prev = Poly polynomial -- Return leaf expression

walk err prev = error(show("Walk error case: ", err))

--- --- Helper functions
litteral n = Poly (Scale n (PolyPower "litteral" 0))    -- looks like Poly (Scale (n % 1) (PolyPower "litteral" 0)) where n is the litteral value
neg expr = Mult (litteral (0-1)) expr                   -- negates an expression, looks like Mult (Poly (Scale ((-1) % 1) (PolyPower "litteral" 0))) (Poly (Scale (2 % 1) (PolyPower "litteral" 0))) meaning -2
sin2 var = Mult (Sin (Poly (Def var))) (Sin (Poly (Def var)))
cos2 var = Mult (Cos (Poly (Def var))) (Cos (Poly (Def var)))
power x n = Poly (PolyPower x n)

derive (Poly (PolyPower var 1)) = litteral 1
derive (Poly (PolyPower var pow)) = Poly (Scale (fromIntegral pow) (PolyPower var (fromIntegral (pow - 1))))
derive (Poly (Scale con (PolyPower var 0))) = litteral 0
derive (Poly (Scale con (PolyPower var 1))) = litteral con
derive (Div (Poly (Scale 1 (PolyPower "litteral" 0))) (Poly (PolyPower var 1))) = Div (litteral 1) (Poly (PolyPower var 2))
derive (Diff x expr) = derive(reduce(expr))

derive (Cos expr) = Cos expr
derive (Sin expr) = Sin expr

derive err = error(show("Derive error case: ", err))
