let a = AND (Gt (Times (Plus (Var "A2") (Const 9.8)) (Const 9.76)) (Var "A4")) (NOT (Eq (Var "HOLA") (Const 8)))
let x = Cond (a) (Seq ([(Empty "B2")]++[(Input "A6")])) (Seq ((Empty "H4"):[]))
Loop a (Seq x:[])