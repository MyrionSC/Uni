module Main where

import qualified HappyParser
import Eval
import Expr














runEvalWith :: (String -> Expr) -> String -> IO ()
runEvalWith parseExpr input = do
  let ast = parseExpr input
  putStrLn $ "AST: " ++ (show ast)
  putStrLn $ "Source: " ++ (source ast)
  putStrLn $ "Eval: " ++ " not implemtented"

main :: IO ()
main = do
  input <- getContents
  putStrLn "Input:"
  putStrLn input
  putStrLn "\nUsing Happy:"
  runEvalWith HappyParser.parseExpr input