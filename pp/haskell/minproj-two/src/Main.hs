module Main where

import qualified HappyParser
import Expr

runEvalWith :: (String -> Expr) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)
  putStrLn ""
--  main

main :: IO ()
main = do
  putStrLn "Input:"
  input <- getContents
  putStrLn input
  runEvalWith HappyParser.parseExpr input
