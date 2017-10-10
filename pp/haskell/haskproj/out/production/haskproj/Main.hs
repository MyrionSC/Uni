module Main where
import Lib

---- lecture 1

-- subject 3

myrev :: [any] -> [any]
myrev a = reverse a

-- subject 6

lastL lst = if length lst == 1 then head lst else lastL lst

-- subject 7

flatten :: [[any]] -> [any]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

duplfirst (x:xs) = x:x:xs















main = print "done"
