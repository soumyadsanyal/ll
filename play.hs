{-# LANGUAGE GADTs #-}
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

data Value = VCustomBool CustomBool  | VNat Nat | VInt Int | VBool Bool | VFunction (Value -> Value)

data Nat = Zero | Succ Nat 
 deriving (Show, Eq)

data CustomBool = CTrue | CFalse
 deriving (Show, Eq)

addnats :: Nat -> Nat -> Nat
addnats x Zero = x
addnats x (Succ y) = (Succ (addnats (x) (y)))

multiplynats :: Nat -> Nat -> Nat
multiplynats x Zero = Zero
multiplynats x (Succ y) = addnats x (multiplynats x y)

customand :: CustomBool -> CustomBool -> CustomBool
customand CTrue CTrue = CTrue
customand _ _ = CFalse

customor :: CustomBool -> CustomBool -> CustomBool
customor CFalse CFalse = CFalse
customor _ _ = CTrue

customnot :: CustomBool -> CustomBool 
customnot CTrue = CFalse
customnot CFalse = CTrue

translateint :: Nat -> Int
translateint Zero = 0
translateint (Succ x) = (translateint x) + 1

translatebool :: CustomBool -> Bool
translatebool CTrue = True
translatebool CFalse = not (translatebool (customnot CFalse))

instance Show Value where
 show (VInt x) = show x
 show (VNat x) = show x
 show (VBool x) = show x
 show (VCustomBool x) = show x
 show _ = "<Function>"

data Var = Var Int
 deriving (Eq, Show)

-- just as a comment, eval is a functor!
eval :: Exp -> Value
eval (Constant x) = x
eval (Plus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x+y)
  (VNat x, VNat y) -> VNat (addnats x y)
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or Nats!"
eval (Minus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x-y)
  (VNat x, VNat y) -> error "subtraction not defined on nats!"
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints!"
eval (Times x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x*y)
  (VNat x, VNat y) -> VNat (multiplynats x y)
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or Nats!"
eval (Divide x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> if (y/=0) then VInt (div x y) else error "Division by zero!"
  (VNat x, VNat y) -> error "Division not defined on Nats!"
  (VInt _, VNat _) -> error "Incompatible argument types!"
  (VNat _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be integers!"
eval (Not x) = case (eval x) of
 (VBool x) -> VBool (not x)
 (VCustomBool x) -> VCustomBool (customnot x)
 _        -> error "Not Boolean!"
eval (And x y) = case (eval x, eval y) of
 (VBool x, VBool y) -> VBool(x && y)
 (VCustomBool x, VCustomBool y) -> VCustomBool (customand x y)
 _         -> error "Not Boolean!"
eval (Or x y) = case (eval x, eval y) of
 (VBool x, VBool y) -> VBool(x ||  y)
 (VCustomBool x, VCustomBool y) -> VCustomBool (customor x y)
 _         -> error "Not Boolean!"
eval (Branch x y z) = case (eval x, eval y, eval z) of
 (VBool x, this, that) -> if (x==True) then this else that
 (VCustomBool x, this, that) -> if (x==CTrue) then this else that
 _         -> error "Condition must evaluate to Bool or CustomBool!"
eval (App first second) = case (eval first) of
 (VFunction f) -> f (eval second)
 _ ->  error "First argument is not a function!"
eval (Fun v f) = VFunction (\x -> eval(subst f v x))


subst :: Exp -> Var -> Value -> Exp
subst c@(Constant _) _ _ = c
subst (Variable v) (Var v') x = if (v==v') then (Constant x) else (Variable v)
subst (Plus m n) v x = Plus (subst m v x) (subst n v x)
subst (Minus m n) v x = Minus (subst m v x) (subst n v x)
subst (Times m n) v x = Times (subst m v x) (subst n v x)
subst (Divide m n) v x = Divide (subst m v x) (subst n v x)
subst (Not m) v x = Not (subst m v x)
subst (And m n) v x = And (subst m v x) (subst n v x)
subst (Or m n) v x = Or (subst m v x) (subst n v x)
subst (App m n) v x = App (subst m v x) (subst n v x)
subst (Fun v' b) v x = if (v==v') then (Fun v' b) else (Fun v' (subst b v x))

plusone = Fun (Var 1) (Plus (Variable 1) (Constant (VNat (Succ Zero))))

y=Fun (Var 1) (App ((Fun (Var 2) (App (Variable 1) (App (Variable 2) (Variable 2))))) (Fun (Var 3) (App (Variable 1) (App (Variable 3) (Variable 3)))))


