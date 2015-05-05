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

data Value = VCustomBool CustomBool  | VCustomInt CustomInt | VInt Int | VBool Bool | VFunction (Value -> Value)

data CustomInt = Zero | Succ CustomInt | Pred CustomInt
 deriving (Show, Eq)

data CustomBool = CTrue | CFalse
 deriving (Show, Eq)

simplifyCustomInts :: CustomInt -> CustomInt
simplifyCustomInts Zero = Zero
simplifyCustomInts (Succ (Pred x)) = x
simplifyCustomInts (Pred (Succ x)) = x
simplifyCustomInts x = x

customadd :: CustomInt -> CustomInt -> CustomInt
customadd x Zero = simplifyCustomInts x
customadd x (Succ y) = simplifyCustomInts (Succ (customadd (simplifyCustomInts x) (simplifyCustomInts y)))
customadd x (Pred y) = simplifyCustomInts (Pred (customadd (simplifyCustomInts x) (simplifyCustomInts y)))

customsubtract:: CustomInt -> CustomInt -> CustomInt
customsubtract x Zero = x
customsubtract x (Succ y) = Pred (customsubtract x y)
customsubtract x (Pred y) = Succ (customsubtract x y)

custommultiply :: CustomInt -> CustomInt -> CustomInt
custommultiply x Zero = Zero
custommultiply x (Succ y) = customadd x (custommultiply x y)
custommultiply x (Pred y) = customsubtract (custommultiply x y) x

customand :: CustomBool -> CustomBool -> CustomBool
customand CTrue CTrue = CTrue
customand _ _ = CFalse

customor :: CustomBool -> CustomBool -> CustomBool
customor CFalse CFalse = CFalse
customor _ _ = CTrue

customnot :: CustomBool -> CustomBool 
customnot CTrue = CFalse
customnot CFalse = CTrue

instance Show Value where
 show (VInt x) = show x
 show (VCustomInt x) = show x
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
  (VCustomInt x, VCustomInt y) -> VCustomInt (customadd x y)
  (VInt _, VCustomInt _) -> error "Incompatible argument types!"
  (VCustomInt _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or CustomInts!"
eval (Minus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x-y)
  (VCustomInt x, VCustomInt y) -> VCustomInt (customsubtract x y)
  (VInt _, VCustomInt _) -> error "Incompatible argument types!"
  (VCustomInt _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or CustomInts!"
eval (Times x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x*y)
  (VCustomInt x, VCustomInt y) -> VCustomInt (custommultiply x y)
  (VInt _, VCustomInt _) -> error "Incompatible argument types!"
  (VCustomInt _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or CustomInts!"
eval (Divide x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> if (y/=0) then VInt (div x y) else error "Division by zero!"
  (VCustomInt x, VCustomInt y) -> error "Division not defined on CustomInts!"
  (VInt _, VCustomInt _) -> error "Incompatible argument types!"
  (VCustomInt _, VInt _) -> error "Incompatible argument types!"
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

plusone = Fun (Var 1) (Plus (Variable 1) (Constant (VCustomInt (Succ Zero))))

y=Fun (Var 1) (App ((Fun (Var 2) (App (Variable 1) (App (Variable 2) (Variable 2))))) (Fun (Var 3) (App (Variable 1) (App (Variable 3) (Variable 3)))))


