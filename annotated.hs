{-# LANGUAGE GADTs #-}
import Debug.Trace

-- Define the language
data Exp where 
 Fun :: Var -> Exp -> Exp
 App :: Exp -> Exp -> Exp 
 Constant :: Value -> Exp 
 Plus :: Exp -> Exp -> Exp 
 Minus :: Exp -> Exp -> Exp
 Times :: Exp -> Exp -> Exp 
 Divide :: Exp -> Exp -> Exp 
 Variable :: Int -> [String] -> Exp
 Not :: Exp -> Exp
 And :: Exp -> Exp -> Exp
 Or :: Exp -> Exp -> Exp
 Branch :: Exp -> Exp -> Exp -> Exp

data Value = VBool' Bool'  | VInt' Int' | VInt Int | VBool Bool | VFunction (Value -> Value)

--Custom datatypes. These will support ad-hoc polymorphism alongside their usual counterparts below.

data Int' = Zero | Succ Int' | Pred Int'
 deriving (Show, Eq)

data Bool' = True' | False'
 deriving (Show, Eq)

data Parity = P | S 

instance Show Value where
 show (VInt x) = show x
 show (VInt' x) = show x
 show (VBool x) = show x
 show (VBool' x) = show x
 show _ = "<Function>"

data Var = Var Int [String]
 deriving (Eq, Show)

-- reducer, reduce, expand and simplify reduce any Int' to a canonical form. This solution only uses the List datatype provided by Haskell.
reducer :: Int' -> Int'
reducer Zero = Zero
reducer (Succ x) = case x of
 (Succ y) -> Succ (reducer x)
 (Pred y) -> reducer y
 Zero -> Succ Zero
reducer (Pred x) = case x of
 (Succ y) -> reducer y
 (Pred y) -> Pred (reducer x)
 Zero -> Pred Zero

expand :: [Parity] -> Int' -> Int'
expand [] x = x
expand (P:rest) x = expand rest (Pred x)
expand (S:rest) x = expand rest (Succ x)

reduce :: [Parity] -> (Int' -> Int') -> Int' -> Int'
--base cases
reduce [] reducer Zero = Zero 
reduce [] reducer (Succ x) = reduce (S:[]) reducer x
reduce [] reducer (Pred x) = reduce (P:[]) reducer x
reduce (S:rest) reducer Zero = expand rest (Succ Zero)
reduce (P:rest) reducer Zero = expand rest (Pred Zero)
--inductive cases
reduce (S:rest) reducer (Pred x) = reduce rest reducer x
reduce (P:rest) reducer (Succ x) = reduce rest reducer x
reduce (S:rest) reducer (Succ x) = reduce (S:S:rest) reducer x
reduce (P:rest) reducer (Pred x) = reduce (P:P:rest) reducer x

simplify :: Int' -> Int'
simplify x = reduce [] reducer x
-- makelist', reduce', expand', simplify' reduce any Int' to canonical form. This alternative solution uses the List and Int types in Haskell.
makelist' :: Int' -> [Char]
makelist' Zero = []
makelist' (Succ x) = 'S':(makelist' x)
makelist' (Pred x) = 'P':(makelist' x)

reduce' :: [Char] -> Int
reduce' [] = 0
reduce' ('S':rest) = 1+(reduce' rest)
reduce' ('P':rest) = (reduce' rest) - 1

expand' :: Int -> Int'
expand' n 
 | n==0 = Zero
 | n>0 = Succ (expand' (n-1))
 | n<0 = Pred (expand' (n+1))

simplify' :: Int' -> Int' 
simplify' x = expand' (reduce' (makelist' x)) 


-- provide operations for Int's. Int's with these operations form a ring isomorphic to the integers. It might be fun to implement the field of fractions.
add' :: Int' -> Int' -> Int'
add' x Zero = simplify x
add' x (Succ y) = simplify (Succ (add' (x) (y)))
add' x (Pred y) = simplify (Pred (add' x y))

minus' :: Int' -> Int' -> Int'
minus' x Zero = simplify x
minus' x (Succ y) = simplify (Pred (minus' (x) (y)))
minus' x (Pred y) = simplify (Succ (minus' x y))


times' :: Int' -> Int' -> Int'
times' x Zero = Zero
times' x (Succ y) = simplify (add' x (times' x y))
times' x (Pred y) = simplify (minus' (times' x y) x)

-- provide operations for Bool'. This gives join, meet and complementation.
and' :: Bool' -> Bool' -> Bool'
and' True' True' = True'
and' _ _ = False'

or' :: Bool' -> Bool' -> Bool'
or' False' False' = False'
or' _ _ = True'

not' :: Bool' -> Bool' 
not' True' = False'
not' False' = True'

--Functions to extract Int's and Bool's from the Value wrapper. Along with translateint and translatebool below, this allows me to map customtypes to their usual counterparts, for the purposes of the talk.
unwrapint :: Value -> Int'
unwrapint x = case x of
 (VInt' Zero) -> Zero
 (VInt' (Succ x)) -> Succ (x)
 (VInt' (Pred x)) -> Pred (x)

unwrapbool :: Value -> Bool'
unwrapbool x = case x of
 (VBool' True') -> True'
 (VBool' False') -> False'

translateint :: Int' -> Int
translateint Zero = 0
translateint (Succ x) = (translateint x) + 1
translateint (Pred x) = (translateint x) - 1

translatebool :: Bool' -> Bool
translatebool True' = True
translatebool False' = not (translatebool (not' False'))

-- just as a comment, eval is a functor!
eval :: Exp -> Value
eval (Constant x) = x
eval (Plus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x+y)
  (VInt' x, VInt' y) -> VInt' (add' (simplify x) (simplify y))
  (VInt _, VInt' _) -> error "Incompatible argument types!"
  (VInt' _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or Int's!"
eval (Minus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x-y)
  (VInt' x, VInt' y) -> VInt' (minus' (simplify x) (simplify y))
  (VInt _, VInt' _) -> error "Incompatible argument types!"
  (VInt' _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints!"
eval (Times x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x*y)
  (VInt' x, VInt' y) -> VInt' (times' (simplify x) (simplify y))
  (VInt _, VInt' _) -> error "Incompatible argument types!"
  (VInt' _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or Int's!"
eval (Divide x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> if (y/=0) then VInt (div x y) else error "Division by zero!"
  (VInt' x, VInt' y) -> error "Division not defined on Int's!"
  (VInt _, VInt' _) -> error "Incompatible argument types!"
  (VInt' _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be integers!"
eval (Not x) = case (eval x) of
 (VBool x) -> VBool (not x)
 (VBool' x) -> VBool' (not' x)
 _        -> error "Not Boolean!"
eval (And x y) = case (eval x, eval y) of
 (VBool x, VBool y) -> VBool(x && y)
 (VBool' x, VBool' y) -> VBool' (and' x y)
 _         -> error "Not Boolean!"
eval (Or x y) = case (eval x, eval y) of
 (VBool x, VBool y) -> VBool(x ||  y)
 (VBool' x, VBool' y) -> VBool' (or' x y)
 _         -> error "Not Boolean!"
eval (Branch x y z) = case (eval x, eval y, eval z) of
 (VBool x, this, that) -> if (x==True) then this else that
 (VBool' x, this, that) -> if (x==True') then this else that
 _         -> error "Condition must evaluate to Bool or Bool'!"
eval (App first second) = case (eval first) of
 (VFunction f) -> trace("evaluating an App") f (eval second)
 _ ->  error "First argument is not a function!"
eval (Fun (Var v annotation) f) = case annotation of 
 (first:second) -> trace("evaluating a Fun") VFunction (\x -> eval(subst f (Var v (first:[])) x)) 
 _ -> error "Types don't match"

subst :: Exp -> Var -> Value -> Exp
subst c@(Constant _) _ _ = trace("subst a constant") c
subst (Variable v annotation) (Var v' annotation') x = if (annotation==annotation') then (if (v==v') then trace("made a substitution for a variable") (Constant x) else trace("variables don't match, skippped subs") Variable v annotation) else trace("annotations didn't match") error "Incompatible types!!!"
subst (Plus m n) v x = trace("subst a Plus") Plus (subst m v x) (subst n v x)
subst (Minus m n) v x = trace("subst a Minus") Minus (subst m v x) (subst n v x)
subst (Times m n) v x = trace("subst a Times") Times (subst m v x) (subst n v x)
subst (Divide m n) v x = trace("subst a Divide") Divide (subst m v x) (subst n v x)
subst (Not m) v x = trace("subst a Not") Not (subst m v x)
subst (And m n) v x = trace("subst a And") And (subst m v x) (subst n v x)
subst (Or m n) v x = trace("subst a Or") Or (subst m v x) (subst n v x)
subst (App m n) v x = trace("subst an App") App (subst m v x) (subst n v x)
subst (Fun (Var v' annotation') b) (Var v annotation) x = if (annotation==annotation') then (if (v==v') then trace("subst an unbound variable") (Fun (Var v' annotation') b) else trace("skipped subst an unbound variable") (Fun (Var v' annotation') (subst b (Var v annotation) x))) else trace("annotations mismatched") error "Not comp types"

plusone = Fun (Var 1 ["Int"]) (Plus (Variable 1 ["Int"]) (Constant (VInt 1)))

timesfour = Fun (Var 1 ["Int"]) (Times (Variable 1 ["Int"]) (Constant (VInt' (Succ (Succ (Succ (Succ Zero)))))))


y=Fun (Var 1 ["Int"]) (App ((Fun (Var 2 ["Int"]) (App (Variable 1 ["Int"]) (App (Variable 2 ["Int"]) (Variable 2 ["Int"]))))) (Fun (Var 3 ["Int"]) (App (Variable 1 ["Int"]) (App (Variable 3 ["Int"]) (Variable 3 ["Int"])))))

test = translateint (unwrapint (eval (Times (Constant (VInt' (Succ (Pred (Succ (Pred (Succ (Succ Zero)))))))) (Constant (VInt' (Pred Zero))))))

twice = Fun (Var 1 ["Int", "Int"]) (
                Fun (Var 2 ["Int"]) (
                          App (Variable 1 ["Int","Int"]) (
                                App (Variable 1 ["Int","Int"]) (
                                    (Variable 2 ["Int"])))))


-- basically the problem here is parsing the type annotations correctly and passing them along. This needs to be done more carefully.



