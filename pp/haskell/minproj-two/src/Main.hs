module Main where

import qualified HappyParser
import Expr

runEvalWith :: (String -> Expr) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)

main :: IO ()
main = do
  input <- getContents
  putStrLn "Input:"
  putStrLn input
  runEvalWith HappyParser.parseExpr input
  putStrLn ""