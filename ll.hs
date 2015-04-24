data Exp = Fun Var Exp | App Exp Exp | ConstantInt Int | Plus Exp Exp | Time Exp Exp
data Var = Var Int

eval :: Exp -> Int
eval (ConstantInt x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Times x y) = (eval x) * (eval y)
eval 


data Nat = Zero | Succ Nat
 deriving (Show, Eq)

adding :: Nat -> Nat -> Nat
adding whatever Zero = whatever
adding whatever (Succ whoever) = Succ (adding whatever whoever)


