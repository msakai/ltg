module Lambda where

import LTG

data Term
    = Var Int
    | Card Card
    | App Term Term
    | Lambda Int Term
    deriving Show

removeVar :: Int -> Term -> Term
removeVar x (Var y) | x==y = (Card I)
removeVar x (App f y) = App (App (Card S) (removeVar x f)) (removeVar x y)
removeVar x (Lambda y z) = removeVar x (removeVar y z)
removeVar x y = App (Card K) y

{-
ski' :: Term -> Term
ski' (App x y)    = App (ski' x) (ski' y)
ski' (Lambda x (Var y))
  | x == y    = I
  | otherwise = App K (Var y)
ski' (Lambda x (App y z)) = App (App S (ski' (Lambda x y))) (ski' (Lambda x z))
ski' (Lambda x (Lambda y z)) = ski' (Lambda x (ski' (Lambda y z)))
ski' (Lambda x y) = App K (ski' y)
ski' x = x
-}

f :: Term -> String
f (App x y@(App _ _)) = f x ++ "(" ++ f y ++ ")"
f (App x y) = f x ++ f y
f (Card c) = show c

{-
f (removeVar 0 (Lambda 0 (Lambda 1 (Lambda 2 (App (Var 0) (App (Var 1) (Var 2)))))))
-}

i x = x
k x y = x
s x y z = (x z) (y z)

kiss = k i s s


