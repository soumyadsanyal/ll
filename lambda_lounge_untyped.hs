{-# LANGUAGE GADTs #-}
data Exp where 
 Fun :: Var -> Exp -> Exp
 App :: Exp -> Exp -> Exp 
 Constant :: Value -> Exp 
 Plus :: Exp -> Exp -> Exp 
 Times :: Exp -> Exp -> Exp 
 Variable :: Int -> Exp

data Value = VInt Int | VBool Bool | VFunction (Value -> Value)

instance Show Value where
 show (VInt x) = show x
 show (VBool x) = show x
 show _ = "<Function>"

data Var = Var Int
 deriving (Eq, Show)

-- just as a comment, eval is a functor!
eval :: Exp -> Value
eval (Constant x) = x
eval (Plus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x+y)
  _                -> error "Not integers, you dumbass!"
eval (Times x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x*y)
  _                -> error "Not integers, you dumbass!"

eval (App first second) = case (eval first) of
 (VFunction f) -> f (eval second)
 _ ->  error "What the flip"
eval (Fun v f) = VFunction (\x -> eval(subst f v x))

subst :: Exp -> Var -> Value -> Exp
subst c@(Constant _) _ _ = c
subst (Variable v) (Var v') x = if (v==v') then (Constant x) else (Variable v)
subst (Plus m n) v x = Plus (subst m v x) (subst n v x)
subst (Times m n) v x = Times (subst m v x) (subst n v x)
subst (App m n) v x = App (subst m v x) (subst n v x)
subst (Fun v' b) v x = if (v==v') then (Fun v' b) else (Fun v' (subst b v x))

plusone = Fun (Var 1) (Plus (Variable 1) (Constant (VInt 1)))






