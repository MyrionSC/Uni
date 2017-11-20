module Main where



--- Data type

-- e ::= p | e1 + e2 | e1 · e2 | e1/e2 | d/dx(e1) | sin e | cos e
-- p ::= x | c · p1 | p1 + p2 | x^k
-- x -> var, k -> range of non-negative ints, c -> konstant






--- Manipulation functions of univariate expressions

-- Addition (e1 + e2)

-- Multiplication (e1 * e2)

-- Multiplication by scalar (e * scalar)

-- Long Division (You can divide exponents. example x⁴ / x² = x²

-- Differentiation



--- Subject to identities:

-- x^m * x^n = x^m+n                  (3)
-- x^m / x^n = x^m−n                  (4)
-- sin2 x + cos2 x = 1                (5)
-- e1 + e2 = e2 + e1                  (6)
-- e · e = e^2                        (7)
-- (e1 + e2) + e3 = e1 + (e2 + e3)    (8)
-- e1 · e2 = e2 · e1                  (9)
-- (e1 · e2) · e3 = e1 · (e2 · e3)    (10)
-- e1 · (e2 + e3) = e1 · e2 + e1 · e3 (11)
-- e1 · 1 = e1                        (12)


--- should evaluate to

-- 1 · (sin x · sin x + cos x · cos x) · x^3 / x^2 -> x

-- d/dx(x^4 + d/dx(cos x)) -> 4x^3 - cos x



--- prettyprint reduction sequences and individual expressions in latex



--- CLI for using the structure and functions




main :: IO ()
main = putStrLn "Lol"


