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

data Value = VCustomBool CustomBool  | VCustomInt CustomInt | VInt Int | VBool Bool | VFunction (Value -> Value)

--Custom datatypes. These will support ad-hoc polymorphism alongside their usual counterparts below.

data CustomInt = Zero | Succ CustomInt | Pred CustomInt
 deriving (Show, Eq)

data CustomBool = CTrue | CFalse
 deriving (Show, Eq)

data Parity = P | S 

-- reducer, reduce, expand and simplifyCustomInts work to reduce any Nat to a canonical form. This solution only uses the List datatype provided by Haskell.
reducer :: CustomInt -> CustomInt
reducer Zero = Zero
reducer (Succ x) = case x of
 (Succ y) -> Succ (reducer x)
 (Pred y) -> reducer y
 Zero -> Succ Zero
reducer (Pred x) = case x of
 (Succ y) -> reducer y
 (Pred y) -> Pred (reducer x)
 Zero -> Pred Zero


reduce :: [Parity] -> (CustomInt -> CustomInt) -> CustomInt -> CustomInt
reduce [] reducer Zero = Zero 
reduce [] reducer (Succ x) = reduce (S:[]) reducer x
reduce [] reducer (Pred x) = reduce (P:[]) reducer x
reduce (S:rest) reducer Zero = expand rest (Succ Zero)
reduce (P:rest) reducer Zero = expand rest (Pred Zero)
reduce (S:rest) reducer (Pred x) = reduce rest reducer x
reduce (P:rest) reducer (Succ x) = reduce rest reducer x
reduce (S:rest) reducer (Succ x) = reduce (S:S:rest) reducer x
reduce (P:rest) reducer (Pred x) = reduce (P:P:rest) reducer x

expand :: [Parity] -> CustomInt -> CustomInt
expand [] nat = nat
expand (P:rest) nat = expand rest (Pred nat)
expand (S:rest) nat = expand rest (Succ nat)

simplifyCustomInts :: CustomInt -> CustomInt
simplifyCustomInts nat = reduce [] reducer nat

-- nattolist, reducelist, expandlist and simplify are an alternative solution to reduce any Nat to a canonical form using the Int datatype provided by Haskell
nattolist :: CustomInt -> [Char]
nattolist Zero = []
nattolist (Succ x) = 'S':(nattolist x)
nattolist (Pred x) = 'P':(nattolist x)

reducelist :: [Char] -> Int
reducelist [] = 0
reducelist ('S':rest) = 1+(reducelist rest)
reducelist ('P':rest) = (reducelist rest) - 1

expandlist :: Int -> CustomInt
expandlist n 
 | n==0 = Zero
 | n>0 = Succ (expandlist (n-1))
 | n<0 = Pred (expandlist (n+1))

simplify :: CustomInt -> CustomInt 
simplify x = expandlist (reducelist (nattolist x)) 


-- provide operations for CustomInts. CustomInts with these operations form a ring isomorphic to the integers. It might be fun to implement the field of fractions.
addCustomInts :: CustomInt -> CustomInt -> CustomInt
addCustomInts x Zero = simplifyCustomInts x
addCustomInts x (Succ y) = simplifyCustomInts (Succ (addCustomInts (x) (y)))
addCustomInts x (Pred y) = simplifyCustomInts (Pred (addCustomInts x y))

minusCustomInts :: CustomInt -> CustomInt -> CustomInt
minusCustomInts x Zero = simplifyCustomInts x
minusCustomInts x (Succ y) = simplifyCustomInts (Pred (minusCustomInts (x) (y)))
minusCustomInts x (Pred y) = simplifyCustomInts (Succ (minusCustomInts x y))


timesCustomInts :: CustomInt -> CustomInt -> CustomInt
timesCustomInts x Zero = Zero
timesCustomInts x (Succ y) = simplifyCustomInts (addCustomInts x (timesCustomInts x y))
timesCustomInts x (Pred y) = simplifyCustomInts (minusCustomInts (timesCustomInts x y) x)

-- provide operations for CustomBool. This gives join, meet and complementation.
customand :: CustomBool -> CustomBool -> CustomBool
customand CTrue CTrue = CTrue
customand _ _ = CFalse

customor :: CustomBool -> CustomBool -> CustomBool
customor CFalse CFalse = CFalse
customor _ _ = CTrue

customnot :: CustomBool -> CustomBool 
customnot CTrue = CFalse
customnot CFalse = CTrue

--Functions to extract CustomInts and CustomBools from the Value wrapper. Along with translateint and translatebool below, this allows me to map customtypes to their usual counterparts, for the purposes of the talk.
unwrapint :: Value -> CustomInt
unwrapint x = case x of
 (VCustomInt Zero) -> Zero
 (VCustomInt (Succ x)) -> Succ (x)
 (VCustomInt (Pred x)) -> Pred (x)

unwrapbool :: Value -> CustomBool
unwrapbool x = case x of
 (VCustomBool CTrue) -> CTrue
 (VCustomBool CFalse) -> CFalse

translateint :: CustomInt -> Int
translateint Zero = 0
translateint (Succ x) = (translateint x) + 1
translateint (Pred x) = (translateint x) - 1

translatebool :: CustomBool -> Bool
translatebool CTrue = True
translatebool CFalse = not (translatebool (customnot CFalse))

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
  (VCustomInt x, VCustomInt y) -> VCustomInt (addCustomInts (simplifyCustomInts x) (simplifyCustomInts y))
  (VInt _, VCustomInt _) -> error "Incompatible argument types!"
  (VCustomInt _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints or CustomInts!"
eval (Minus x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x-y)
  (VCustomInt x, VCustomInt y) -> VCustomInt (minusCustomInts (simplifyCustomInts x) (simplifyCustomInts y))
  (VInt _, VCustomInt _) -> error "Incompatible argument types!"
  (VCustomInt _, VInt _) -> error "Incompatible argument types!"
  _                -> error "Arguments must be Ints!"
eval (Times x y) = case (eval x, eval y) of 
  (VInt x, VInt y) -> VInt (x*y)
  (VCustomInt x, VCustomInt y) -> VCustomInt (timesCustomInts (simplifyCustomInts x) (simplifyCustomInts y))
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
timesfour = Fun (Var 1) (Times (Variable 1) (Constant (VCustomInt (Succ (Succ (Succ (Succ Zero)))))))


y=Fun (Var 1) (App ((Fun (Var 2) (App (Variable 1) (App (Variable 2) (Variable 2))))) (Fun (Var 3) (App (Variable 1) (App (Variable 3) (Variable 3)))))

test = translateint (unwrapint (eval (Times (Constant (VCustomInt (Succ (Pred (Succ (Pred (Succ (Succ Zero)))))))) (Constant (VCustomInt (Pred Zero))))))

