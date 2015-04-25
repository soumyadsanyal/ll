{-# LANGUAGE GADTs #-}
data Exp a where 
 Fun :: (Exp a -> Exp b) -> (Exp (a->b))
 App :: Exp (a->b) -> Exp a -> Exp b   
 Constant :: a -> Exp a
 Plus :: Exp Int -> Exp Int -> Exp Int
 Times :: Exp Int -> Exp Int -> Exp Int

eval :: Exp a -> a
eval (Constant x) = x
eval (Plus x y) = (eval x) + (eval y)
eval (Times x y) = (eval x) * (eval y)
eval (App first second) = (eval first) (eval second)
eval (Fun f) = (\x -> eval (f (Constant x)  ))

plusone = Fun (\x -> (Plus x (Constant 1)))


data Nat = Zero | Succ Nat
 deriving (Show, Eq)

adding :: Nat -> Nat -> Nat
adding whatever Zero = whatever
adding whatever (Succ whoever) = Succ (adding whatever whoever)


