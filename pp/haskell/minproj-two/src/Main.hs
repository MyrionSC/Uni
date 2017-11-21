module Main where

import HappyParser
import Expr

--getFirst :: [[String]] -> [String]
--getFirst (x:xs) = x
--getSecond (x:xs) = getFirst xs


solve :: Expr -> IO ()
solve ast = do
  putStrLn $ "In: " ++ printExpr ast
  let reduced = reduce ast
  if ast == reduced
    then putStrLn "No reduction done"
    else putStrLn $ "Reduced to: " ++ printExpr reduced
  putStrLn ""




main :: IO ()
main = do
--  putStrLn "Input:"
  input <- getContents
  putStrLn input
  let ast = parseExpr input
  solve ast
