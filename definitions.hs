type Ident = String

data Command a = Assign Ident (NExpr a)  | Input Ident | Print (NExpr a) |
				Empty Ident | Push Ident (NExpr a) | Pop Ident Ident | Size Ident Ident |
				Seq [Command a] | Cond (BExpr a) (Command a) (Command a) | Loop (BExpr a) (Command a)

data BExpr a= AND (BExpr a) (BExpr a) | OR (BExpr a) (BExpr a) | NOT (BExpr a) | Gt (NExpr a) (NExpr a) | Eq (NExpr a) (NExpr a)
data NExpr a= Var Ident | Const a | Plus (NExpr a) (NExpr a) | Minus (NExpr a) (NExpr a) | Times (NExpr a) (NExpr a)

tab = \x ->  replicate x '-'
showaux (Assign x y) i 	= (tab i)++x ++ " := " ++ (show y)++"\n"
showaux (Input x) i 	= (tab i)++"INPUT "++x++"\n"
showaux (Print x) i 	= (tab i)++"PRINT "++(show x)++"\n"
showaux (Empty x) i 	= (tab i)++"EMPTY "++x++"\n"
showaux (Push x y) i 	= (tab i)++"PUSH "++x++" "++(show y)++"\n"
showaux (Pop x y) i 	= (tab i)++"POP "++x++" "++y++"\n"
showaux (Size x y) i 	= (tab i)++"SIZE "++x++" "++y++"\n"
showaux (Cond x a b) i 	= (tab i)++"IF "++(show x)++" THEN\n"++(showaux a (i+1)) ++ (tab i) ++ "ELSE\n"++(showaux b (i+1))++(tab i)++"END\n"
showaux (Seq (x:xs)) i 	= (showaux x i)++(showaux (Seq xs) i)
showaux (Seq []) i 		= ""
showaux (Loop x y) i 	= (tab i)++"WHILE "++(show x)++(tab i)++"\nDO\n"++(showaux y (i+1))++(tab i)++"END\n"

instance Show a => Show (Command a) where
	show x =  showaux x 0

instance Show a => Show (BExpr a) where
	show (AND x y) 	= (show x)++" AND "++(show y)
	show (OR x y) 	= (show x)++" OR "++(show y)
	show (NOT x) 	= "NOT "++(show x)
	show (Gt x y) 	= (show x)++" > "++(show y)
	show (Eq x y) 	= (show x)++" = "++(show y)

instance Show a => Show (NExpr a) where
	show (Var x) 		= x
	show (Const x) 		= (show x)
	show (Plus x y) 	= (show x)++" + "++(show y)
	show (Minus x y) 	= (show x)++" - "++(show y)
	show (Times x y) 	= (show x)++" * "++(show y)



