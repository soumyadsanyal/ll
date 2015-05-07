{-# LANGUAGE GADTs #-}

-- Define the language
data Exp where 
 Fun :: Var -> Exp -> Exp
 App :: Exp -> Exp -> Exp 
 Constant :: Value -> Exp 
 Plus :: Exp -> Exp -> Exp 
 Minus :: Exp -> Exp -> Exp
 Times :: Exp -> Exp -> Exp 
 Divide :: Exp -> Exp -> Exp 
 Variable :: Int -> Exp
 Not :: Exp -> Exp
 And :: Exp -> Exp -> Exp
 Or :: Exp -> Exp -> Exp
 Branch :: Exp -> Exp -> Exp -> Exp

data Value = VNat Nat | VInt Int | VBool Bool | VFunction (Value -> Value) 

--Custom datatypes. 

data Nat = Zero | Succ Nat 
 deriving (Show, Eq)

instance Show Value where
 show (VInt x) = show x
 show (VNat x) = show x
 show (VBool x) = show x
 show _ = "<Function>"

data Var = Var Int Type
 deriving (Eq, Show)

data Type = TInt | TBool | TNat | TFunction Type Type  | TError
 deriving (Eq, Show)


-- provide operations for Nats. 
add' :: Nat -> Nat -> Nat
add' x Zero = x
add' x (Succ y) = (Succ (add' (x) (y)))

times' :: Nat -> Nat -> Nat
times' x Zero = Zero
times' x (Succ y) = (add' x (times' x y))

--Functions to extract Nats from the Value wrapper. Along with translateint below, this allows me to map customtypes to their usual counterparts, for the purposes of the talk.
unwrapint :: Value -> Nat
unwrapint x = case x of
 (VNat Zero) -> Zero
 (VNat (Succ x)) -> Succ (x)

translateint :: Nat -> Int
translateint Zero = 0
translateint (Succ x) = (translateint x) + 1

--define eval
eval :: Exp -> Value
eval (Constant x) = x
eval (Plus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x+y)
  (VNat x, VNat y) -> VNat (add' (x) (y))
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or Nats!"
eval (Minus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x-y)
  (VNat x, VNat y) -> VInt ((translateint x) - (translateint y))
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints!"
eval (Times x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x*y)
  (VNat x, VNat y) -> VNat (times' (x) (y))
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or Nats!"
eval (Divide x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> if (y/=0) then VInt (div x y) else error "Division by zero!"
  (VNat x, VNat y) -> if (y/= Zero) then VInt (div (translateint x) (translateint y)) else error "Division by zero!"
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be integers!"
eval (Not x) = case (eval x) of
 (VBool x) -> VBool (not x)
 _        -> error "Not Boolean!"
eval (And x y) = case (eval x, eval y) of
 (VBool x, VBool y) -> VBool(x && y)
 _         -> error "Not Boolean!"
eval (Or x y) = case (eval x, eval y) of
 (VBool x, VBool y) -> VBool(x ||  y)
 _         -> error "Not Boolean!"
eval (Branch x y z) = case (eval x, eval y, eval z) of
 (VBool x, this, that) -> if (x==True) then this else that
 _         -> error "Condition must evaluate to Bool or Bool'!"
eval (App first second) = case (eval first) of
 (VFunction f) -> f (eval second)
 _ ->  error "First argument is not a function!"
eval (Fun (Var v _) f) = VFunction (\x -> eval(subst f v x))


subst :: Exp -> Int -> Value -> Exp
subst c@(Constant _) _ _ = c
subst (Variable v ) (v' ) x = if (v==v') then (Constant x) else (Variable v )
subst (Plus m n) v x = Plus (subst m v x) (subst n v x)
subst (Minus m n) v x = Minus (subst m v x) (subst n v x)
subst (Times m n) v x = Times (subst m v x) (subst n v x)
subst (Divide m n) v x = Divide (subst m v x) (subst n v x)
subst (Not m) v x = Not (subst m v x)
subst (And m n) v x = And (subst m v x) (subst n v x)
subst (Or m n) v x = Or (subst m v x) (subst n v x)
subst (App m n) v x = App (subst m v x) (subst n v x)
subst (Fun (Var v' t') b) (v ) x = if (v==v') then (Fun (Var v' t')  b) else (Fun (Var v' t') (subst b (v ) x))

-- this computes the type of an expression
evalT :: (Int -> Type) -> Exp -> Type
evalT _ (Constant (VInt _)) = TInt  
evalT _ (Constant (VBool _)) = TBool
evalT _ (Constant (VNat _)) = TNat
evalT _ (Constant _) = TError
evalT tenv (Variable v) = tenv v
evalT tenv (Plus x y) = case (evalT tenv x, evalT tenv y) of 
  (TInt, TInt ) -> TInt
  (TNat , TNat ) -> TNat 
  _ -> TError
evalT tenv (Minus x y) = case (evalT tenv x, evalT tenv y) of 
  (TInt , TInt ) -> TInt
  (TNat , TNat ) -> TInt
  _ -> TError
evalT tenv (Times x y) = case (evalT tenv x, evalT tenv y) of 
  (TInt , TInt ) -> TInt
  (TNat , TNat ) -> TNat
  _                -> TError
evalT tenv (Divide x y) = case (evalT tenv x, evalT tenv y) of 
  (TInt , TInt ) -> TInt
  (TNat, TNat) -> TInt
  _                -> TError 
evalT tenv (Not x) = case (evalT tenv x) of
 (TBool ) -> TBool 
 _        -> TError
evalT tenv (And x y) = case (evalT tenv x, evalT tenv y) of
 (TBool , TBool ) -> TBool
 _         -> TError
evalT tenv (Or x y) = case (evalT tenv x, evalT tenv y) of
 (TBool , TBool ) -> TBool
 _         -> TError 
evalT tenv (Branch x y z) = case (evalT tenv x, evalT tenv y, evalT tenv z) of
 (TBool , this, that) -> if (this==that) then this else TError
 _         -> TError 
evalT tenv (App f x) = case (evalT tenv f) of
 (TFunction a b) -> if (a==(evalT tenv x)) then b else TError
 _ ->  TError 
evalT tenv (Fun (Var v t) f) = TFunction t (evalT (extend tenv (v) t) f)

extend :: Eq a => (a -> b) -> a -> b -> (a -> b)
extend f x y = \w -> if (w==x) then y else (f w)

-- This is the eval' we'll call
eval' :: Exp -> Value
eval' e = if (evalT initialenv e /= TError) then eval e else error "Type error!"
 where
  initialenv _ = TError

-- predefined expressions for testing
plusone = Fun (Var 1 TNat) (Plus (Variable 1 ) (Constant (VNat (Succ Zero))))
timesfour = Fun (Var 1 TNat) (Times (Variable 1 ) (Constant (VNat (Succ (Succ (Succ (Succ Zero)))))))


y=Fun (Var 1 TNat) (App ((Fun (Var 2 TNat) (App (Variable 1 ) (App (Variable 2 ) (Variable 2 ))))) (Fun (Var 3 TNat) (App (Variable 1 ) (App (Variable 3 ) (Variable 3 )))))


double = Fun (Var 1 (TFunction TNat TNat)) (
             Fun (Var 2 TNat) (
                 App (Variable 1 ) (
                      App (Variable 1 ) (
                              (Variable 2 )))))




--Future work: implement parametric polymorphism and quantification


