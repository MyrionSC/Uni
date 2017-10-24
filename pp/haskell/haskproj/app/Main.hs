module Main where
import Lib


--- Lecture 2

-- ex 1
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x:takeWhile' p xs else takeWhile' p xs

dropWhile' p [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:dropWhile' p xs

-- ex 2
-- t :: (Integer -> [Integer]) -> t
factors n = [i | i <- [1..n], n `mod` i == 0]

-- ex 3

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
fib n = fibs!!n

-- ex4

data N = Zero
       | Succ N
       deriving (Eq, Show)

-- ex 5

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

b = Leaf 2
c = Node (Leaf 1) (Leaf 2)
d = Node c c
e = Node d d

leafNumber (Leaf x) = x
leafNumber (Node l r) = leafNumber l + leafNumber r

balancedTree (Leaf x) = True
balancedTree (Node l r) = abs (leafNumber l - leafNumber r) < 1


-- ex 6

halve xs = splitAt (div (length xs) 2) xs
balance [x] = Leaf x
balance xs = Node (balance ys) (balance zs)
            where (ys,zs) = halve xs

l1 = [1]
l = [1,2,3,4,5,6,7,8,9,10,11]
ls = splitAt (length l) l

-- t1 = Node (b)



-- balanced tree =

-- submission url: http://people.cs.aau.dk/~normark/cgi-bin/QA/form.cgi?event=PP-E17-6&group=8905791203230219750982332211&user-name=marand13


main = print "done"
