module Lambda (
    Name,
    Term(..),
    Context(..),
    i,
    k,
    s,
    y,
    t,
    omega,
    omega2,
    true,
    false,
    pair,
    iter,
    numeral,
    add,
    mult,
    expn,
    predc,
    vars,
    varss,
    newvar,
    free,
    freeIn,
    closed,
    closedUpTo,
    closure,
    subterms,
    sub,
    fill,
    normal,
    normalize,
    reduceLeftOnce,
    reduceLeft,
) where

import qualified Data.Set as Set

unions :: (Ord a) => Set.Set (Set.Set a) -> Set.Set a
unions = foldl Set.union Set.empty

type Name = String

data Term = Var Name | App Term Term | Lam Name Term
    deriving (Ord)

instance Eq Term where
    (Var x) == (Var y)              = x == y
    (App t1 t2) == (App t3 t4)      = t1 == t3 && t2 == t4
    (Lam x t1) == (Lam y t2)
        | x == y                    = t1 == t2
        | otherwise                 = sub x (Var z) t1 == sub y (Var z) t2
        where z = newvar $ Set.fromList [t1, t2]
    _ == _                          = False

instance Show Term where
    show (Var x)        = x
    show (App t1 t2)    = "(" ++ show t1 ++ show t2 ++ ")"
    show (Lam x t)      = "(\\" ++ x ++ "->" ++ show t ++ ")"

data Context = Hole | CVar Name | CApp Context Context | CLam Name Context
    deriving (Eq,Ord)

instance Show Context where
    show Hole           = "[ ]"
    show (CVar x)       = x
    show (CApp c1 c2)   = "(" ++ show c1 ++ show c2 ++ ")"
    show (CLam x c)     = "(\\" ++ x ++ "->" ++ show c ++ ")"

i :: Term
i = Lam "x" $ Var "x"

k :: Term
k = Lam "x" $ Lam "y" $ Var "x"

s :: Term
s = Lam "x" $ Lam "y" $ Lam "z" $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))

y :: Term
y = Lam "f" $ App w w
    where w = Lam "x" $ App (Var "f") (App (Var "x") (Var "x"))

t :: Term
t = App a a
    where a = Lam "x" $ Lam "f" $ App (Var "f") (App (App (Var "x") (Var "x")) (Var "f"))

omega :: Term
omega = Lam "x" $ App (Var "x") (Var "x")

omega2 :: Term
omega2 = App omega omega

true :: Term
true = k

false :: Term
false = App k i

pair :: Term -> Term -> Term
pair t1 t2 = Lam "x" $ App (App (Var "x") t1) t2

iter :: Int -> Term -> Term -> Term
iter n f = head . drop n . iterate (App f)

numeral :: Int -> Term
numeral n = Lam "f" $ Lam "x" $ iter n (Var "f") (Var "x")

add :: Term
add = Lam "x" $ Lam "y" $ Lam "p" $ Lam "q" $ App (App (Var "x") (Var "p")) (App (App (Var "y") (Var "p")) (Var "q"))

mult :: Term
mult = Lam "x" $ Lam "y" $ Lam "z" $ App (Var "x") (App (Var "y") (Var "z"))

expn :: Term
expn = Lam "x" $ Lam "y" $ App (Var "y") (Var "x")

predc :: Term
predc = Lam "x" $ Lam "y" $ Lam "z" $ App (App (App (Var "x") dualsucc) (App k (Var "z"))) i
    where dualsucc = Lam "p" $ Lam "q" $ App (Var "q") (App (Var "p") (Var "y"))

vars :: Term -> Set.Set Name
vars (Var x)        = Set.singleton x
vars (App t1 t2)    = Set.union (vars t1) (vars t2)
vars (Lam x t)      = Set.insert x $ vars t

varss :: Set.Set Term -> Set.Set Name
varss = unions . Set.map vars

newvar :: Set.Set Term -> Name
newvar ts = head $ dropWhile (`Set.member` vs) xs
    where vs = varss ts
          xs = iterate ('x':) "x"

free :: Term -> Set.Set Name
free (Var x)        = Set.singleton x
free (App t1 t2)    = Set.union (free t1) (free t2)
free (Lam x t)      = Set.delete x $ free t

freeIn :: Name -> Term -> Bool
freeIn x = Set.member x . free

closed :: Term -> Bool
closed = Set.null . free

closedUpTo :: Set.Set Name -> Term -> Bool
closedUpTo s = (`Set.isSubsetOf` s) . free

closure :: Term -> Term
closure t = foldr (\v t -> Lam v t) t $ free t

subterms :: Term -> Set.Set Term
subterms t@(Var _)      = Set.singleton t
subterms t@(App t1 t2)  = Set.insert t $ Set.union (subterms t1) (subterms t2)
subterms t@(Lam _ t1)   = Set.insert t $ subterms t1

sub :: Name -> Term -> Term -> Term
sub x s t@(Var y)
    | x == y        = s
    | otherwise     = t
sub x s (App t1 t2) = App (sub x s t1) (sub x s t2)
sub x s t@(Lam y t1)
    | freeIn y s    = error "Invalid substitution!"
    | x == y        = t
    | otherwise     = Lam y (sub x s t1)

fill :: Context -> Term -> Term
fill Hole t             = t
fill (CVar x) _         = Var x
fill (CApp c1 c2) t     = App (fill c1 t) (fill c2 t)
fill (CLam x c) t       = Lam x $ fill c t

normal :: Term -> Bool
normal (Var _)              = True
normal (App (Lam _ _) _)    = False
normal (App t1 t2)          = normal t1 && normal t2
normal (Lam _ t)            = normal t

normalize :: Term -> Term
normalize = head . dropWhile (not . normal) . reduceLeft

reduceLeftOnce :: Term -> Term
reduceLeftOnce t@(Var _)                = t
reduceLeftOnce (App (Lam x t1) t2)      = sub x t2 t1
reduceLeftOnce (App t1 t2)
    | normal t1                         = App t1 (reduceLeftOnce t2)
    | otherwise                         = App (reduceLeftOnce t1) t2
reduceLeftOnce (Lam x t)                = Lam x $ reduceLeftOnce t

reduceLeft :: Term -> [Term]
reduceLeft = iterate reduceLeftOnce
