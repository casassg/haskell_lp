type Ident = String

data Command a = Assign Ident (NExpr a)  | Input Ident | Print (NExpr a) |
                Empty Ident | Push Ident (NExpr a) | Pop Ident Ident | Size Ident Ident |
                Seq [Command a] | Cond (BExpr a) (Command a) (Command a) | Loop (BExpr a) (Command a)

data BExpr a= AND (BExpr a) (BExpr a) | OR (BExpr a) (BExpr a) | NOT (BExpr a) | Gt (NExpr a) (NExpr a) | Eq (NExpr a) (NExpr a)
data NExpr a= Var Ident | Const a | Plus (NExpr a) (NExpr a) | Minus (NExpr a) (NExpr a) | Times (NExpr a) (NExpr a)

tab = \x ->  replicate (x*2) ' '
showaux (Assign x y) i  = (tab i)++x ++ " := " ++ (show y)++"\n"
showaux (Input x) i     = (tab i)++"INPUT "++x++"\n"
showaux (Print x) i     = (tab i)++"PRINT "++(show x)++"\n"
showaux (Empty x) i     = (tab i)++"EMPTY "++x++"\n"
showaux (Push x y) i    = (tab i)++"PUSH "++x++" "++(show y)++"\n"
showaux (Pop x y) i     = (tab i)++"POP "++x++" "++y++"\n"
showaux (Size x y) i    = (tab i)++"SIZE "++x++" "++y++"\n"
showaux (Cond x a b) i  = (tab i)++"IF "++(show x)++" THEN\n"++(showaux a (i+1)) ++ (tab i) ++ "ELSE\n"++(showaux b (i+1))++(tab i)++"END\n"
showaux (Seq (x:xs)) i  = (showaux x i)++(showaux (Seq xs) i)
showaux (Seq []) i      = ""
showaux (Loop x y) i    = (tab i)++"WHILE "++(show x)++"\n"++(tab i)++"DO\n"++(showaux y (i+1))++(tab i)++"END\n"

instance Show a => Show (Command a) where
    show x =  showaux x 0

instance Show a => Show (BExpr a) where
    show (AND x y)  = (show x)++" AND "++(show y)
    show (OR x y)   = (show x)++" OR "++(show y)
    show (NOT x)    = "NOT "++(show x)
    show (Gt x y)   = (show x)++" > "++(show y)
    show (Eq x y)   = (show x)++" = "++(show y)

instance Show a => Show (NExpr a) where
    show (Var x)        = x
    show (Const x)      = (show x)
    show (Plus x y)     = (show x)++" + "++(show y)
    show (Minus x y)    = (show x)++" - "++(show y)
    show (Times x y)    = (show x)++" * "++(show y)



data SymTable a = Elem Ident (Either a [a]) (SymTable a) | NoElem
    deriving Show


addBasic::(SymTable a)->(Ident)->Maybe a->(SymTable a)
addBasic s i Nothing = s
addBasic s i (Just e) = Elem i (Left e) cleaned
    where 
        cleaned = clean s i
        clean a@(Elem inid elem s_r) id
            | inid == id    = clean s_r id
            | otherwise     = Elem inid elem (clean s_r id)
        clean NoElem id = NoElem
addStack::(SymTable a)->(Ident)->Maybe [a]->(SymTable a)
addStack s i Nothing = s
addStack s i (Just e) = Elem i (Right e) cleaned
    where 
        cleaned = clean s i
        clean a@(Elem inid elem s_r) id
            | inid == id    = clean s_r id
            | otherwise     = Elem inid elem (clean s_r id)
        clean NoElem id = NoElem



getBasic::(SymTable a)->(Ident)->(Maybe a)
getBasic (Elem ii (Left e) sub) i 
  | i == ii     = Just e
  | otherwise   = (getBasic sub i)
getBasic (Elem ii (Right e) sub) i = (getBasic sub i)
getBasic (NoElem) i = Nothing
getStack::(SymTable a)->(Ident)->(Maybe [a])
getStack (Elem ii (Right e) sub) i 
  | i == ii     = Just e
  | otherwise   = (getStack sub i)
getStack (Elem ii (Left e) sub) i = (getStack sub i)
getStack (NoElem) i = Nothing
getType:: (SymTable a)->(Ident)->String
getType s id = t_of (getBasic s id) (getStack s id)
    where 
        t_of Nothing Nothing = "None"
        t_of _ Nothing = "Num"
        t_of _ _ = "Stack"

rec_eval :: (c->c->b) -> (Either String c) -> (Either String c)->(Either String b)
rec_eval f (Left s) (Left s2)       = Left (s++"|"++s2)
rec_eval f (Left s) _               = Left s
rec_eval f _ (Left s)               = Left s
rec_eval f (Right a1) (Right a2)    = Right (f a1 a2)

class Evaluable e where
    eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> (Either String a)
    typeCheck :: (Ident -> String) -> (e a) -> Bool


instance Evaluable NExpr where
    eval f (Var s)  = iev (f s)
        where 
            iev Nothing     = Left ("undefined variable "++s)
            iev (Just ev)   = Right ev
    eval f (Const a)    = Right a
    eval f (Plus a e)       = rec_eval (+) (eval f a) (eval f e)
    eval f (Times a e)      = rec_eval (*) (eval f a) (eval f e)
    eval f (Minus a e)      = rec_eval (-) (eval f a) (eval f e)
    typeCheck f (Var s)
        | t == "Stack"    = False
        | otherwise       = True
        where 
            t = f s
    typeCheck f (Const s) = True
    typeCheck f (Plus i1 i2) = (typeCheck f i1) && (typeCheck f i2)
    typeCheck f (Minus i1 i2) = (typeCheck f i1) && (typeCheck f i2) 
    typeCheck f (Times i1 i2) = (typeCheck f i1) && (typeCheck f i2)


fromBool ::  (Num a, Ord a) => (Either String Bool)->Either String a
fromBool (Right True)   = Right 1
fromBool (Right False)  = Right 0
fromBool (Left s)       = Left s
toBool ::  (Num a, Ord a) => (Either String a)->Either String Bool
toBool (Right 1)      = Right True
toBool (Right 0)      = Right False
toBool (Left s)       = Left s
b_eval :: (Num a, Ord a) => (Bool->Bool->Bool)->(Either String a)-> (Either String a)-> (Either String a)
b_eval f a1 a2 = fromBool  (rec_eval (f) (toBool a1) (toBool a2))

instance Evaluable BExpr where
    eval f (AND a1 a2)  = b_eval (&&) (eval f a1) (eval f a2)           
    eval f (OR a1 a2)   = b_eval (||) (eval f a1) (eval f a2)
    eval f (NOT a)      = (iev (eval f a))
        where
            iev a@(Left _)    = a
            iev a     = fromBool (fmap not (toBool a))
    eval f (Gt a1 a2)   = fromBool (rec_eval (>)  (eval f a1) (eval f a2))
    eval f (Eq a1 a2)   = fromBool (rec_eval (==) (eval f a1) (eval f a2))
    typeCheck f (AND a1 a2)     = (typeCheck f a1) && (typeCheck f a2)
    typeCheck f (OR a1 a2)      = (typeCheck f a1) && (typeCheck f a2)
    typeCheck f (NOT a)         = (typeCheck f a)
    typeCheck f (Gt a1 a2)      = (typeCheck f a1) && (typeCheck f a2)
    typeCheck f (Eq a1 a2)      = (typeCheck f a1) && (typeCheck f a2)


getRes::(Either String a)->(Maybe a)
getRes (Right e) = Just e
getRes _ = Nothing

getError::(Either String a)->String
getError (Left a) = a
getError _ = "Si surt mai aquest error es que les condicions no estan be."

isLeft::(Either String a)-> Bool
isLeft (Left a)=True
isLeft _ = False

fromMaybe:: a -> Maybe a -> a
fromMaybe _ (Just e)=e
fromMaybe e _ = e

isNothing::(Maybe a)-> Bool
isNothing Nothing = True
isNothing _ = False

size::(Num a, Ord a) =>[a]->a
size [] = 0
size (x:xs)= 1 + (size xs)


interpretCommand:: (Num a, Ord a) => SymTable a ->[a]-> Command a ->((Either String [a]),SymTable a, [a])

interpretCommand s inp (Assign id expr)
    | (not correct) = ((Left "type error"), s, inp)
    | isLeft evExpr = ((Left error),s,inp)
    | otherwise     = ((Right []), (addBasic s id  res),inp)
    where 
        correct     = typeCheck (getType s) expr
        res         = getRes (evExpr)
        error       = getError (evExpr)
        evExpr      = eval (getBasic s) expr 


interpretCommand s [] (Input id)    = (Left "empty input stack",s,[])
interpretCommand s (x:xs) (Input id) =  ((Right []), addBasic s id (Just x),xs)



interpretCommand s inp (Print expr) 
    | (not correct) = ((Left "type error"), s, inp)
    | isLeft evExpr = ((Left error),s,inp)
    | otherwise     = ((Right [res_out]), s,inp)
    where 
        correct     = typeCheck (getType s) expr
        res_out     = fromMaybe 0 res
        res         = getRes (evExpr)
        error       = getError (evExpr)
        evExpr      = eval (getBasic s) expr 


interpretCommand s inp (Empty id) 
    | isNothing elem    = ((Right []), addStack s id (Just []),inp)
    | otherwise         = ((Left "variable already exists"), s,inp)
    where
        elem = getStack s id


interpretCommand s inp (Push id expr)
    | not correct       = ((Left "type error"), s, inp)
    | isLeft evExpr     = ((Left error),s,inp)
    | isNothing melem   = ((Left "stack doesn't exist"),s,inp)
    | otherwise         = ((Right []),addStack s id (Just (res_out:elem)), inp)
    where 
        melem       = getStack s id
        elem        = fromMaybe [0] melem
        correct     = typeCheck (getType s) expr
        res_out     = fromMaybe 0 res
        res         = getRes (evExpr)
        error       = getError (evExpr)
        evExpr      = eval (getBasic s) expr 

interpretCommand s inp (Pop idS idO)
    | isNothing mstack      = ((Left "stack doesn't exist"),s,inp)
    | null stack            = ((Left "stack is empty"),s,inp)
    | not (isNothing elem)  = ((Left "variable already exists"), s,inp)
    | otherwise             = ((Right []),(addBasic (addStack s idS (Just xs)) idO (Just x)),inp)
    where
        mstack  = getStack s idS
        stack   = fromMaybe [] mstack
        (x:xs)  = stack
        elem    = getBasic s idO

interpretCommand s inp (Size idS idO)
    | isNothing mstack      = ((Left "stack doesn't exist"),s,inp)
    | not (isNothing elem)  = ((Left "variable already exists"), s,inp)
    | otherwise             = ((Right []),(addBasic s idO (Just len)),inp)
    where
        mstack  = getStack s idS
        stack   = fromMaybe [] mstack
        elem    = getBasic s idO
        len     = size stack  

interpretCommand s inp (Seq l)
    | null l         = (Right [],s,inp)
    | isLeft eout1   = a1
    | isLeft eout2   = a2
    | otherwise      = (Right (out1++out2),ns2,ni2)
    where
        (x:xs)              = l
        a1@(eout1, ns1,ni1) = interpretCommand s inp x
        a2@(eout2, ns2,ni2) = interpretCommand ns1 ni1 (Seq xs)
        out1                = fromMaybe [] (getRes eout1)
        out2                = fromMaybe [] (getRes eout2)
interpretCommand s inp (Cond con c1 c2)
    | not correct       = ((Left "type error"), s, inp)
    | isLeft mcert      = ((Left error),s,inp)
    | cert              = interpretCommand s inp c1
    | otherwise         = interpretCommand s inp c2
    where 
        mcert   = toBool (eval (getBasic s) con)
        correct = typeCheck (getType s) con
        cert    = fromMaybe False (getRes mcert)
        error   = getError mcert

interpretCommand s inp c@(Loop con com)
    | not correct       = ((Left "type error"), s, inp)
    | isLeft mcert      = ((Left error),s,inp)
    | not cert          = (Right [],s, inp)
    | otherwise         = interpretCommand ns1 ni1 c
    where 
        mcert   = toBool (eval (getBasic s) con)
        correct = typeCheck (getType s) con
        cert    = fromMaybe False (getRes mcert)
        error   = getError mcert
        (eout1, ns1,ni1) = interpretCommand s inp com

interpretProgram:: (Num a,Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram inp c = out
    where
        (out, _, _) = interpretCommand NoElem inp c