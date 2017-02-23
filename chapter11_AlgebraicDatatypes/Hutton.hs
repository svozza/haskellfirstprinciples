data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n)    = n
eval (Add e e') = (eval e) + (eval e')

printExpr :: Expr -> String
printExpr (Lit n)    = show n
printExpr (Add e e') = (printExpr e) ++ " + " ++ (printExpr e')
