module HuttonsRazor where

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x  ) = x
eval (Add x y) = eval x + eval y

evalTest :: Integer
evalTest = eval (Add (Lit 1) (Lit 9001))

printExpr :: Expr -> String
printExpr (Lit x  ) = show x
printExpr (Add x y) = printExpr x <> " + " <> printExpr y

printExprTest :: String
printExprTest = printExpr (Add (Lit 1) (Lit 9001))
