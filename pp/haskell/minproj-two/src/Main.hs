module Main where

import System.IO

import HappyParser
import Expr

-- reduce untill there are no changes
solve :: Expr -> IO ()
solve ast = do
  let reduced = reduce ast
  if ast == reduced
    then do
      putStrLn $ "Out: " ++ printExpr reduced
      putStrLn $ "Pretty: " ++ prettyPrint reduced
    else solve reduced

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getContents
  putStrLn input
  let ast = parseExpr input
  putStrLn $ "In: " ++ printExpr ast
  solve ast
  putStrLn ""

