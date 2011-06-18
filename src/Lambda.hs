module Lambda
  ( Var
  , Term (..)
  , compile
  ) where

import LTG

type Var = String

data Term
    = Var String
    | Int !Int
    | Card Card
    | App Term Term
    | Lambda Var Term
    deriving Show

{-
TODO: Term‚Å‚Í‚È‚­LTG‚ÌValue‚Ö‚ÆƒRƒ“ƒpƒCƒ‹‚µ‚½‚¢
-}
compile :: Term -> Term
compile (Int i)      = Int i
compile (Card c)     = Card c
compile (App a b)    = app (compile a) (compile b)
compile (Lambda v a) = removeVar v a

removeVar :: Var -> Term -> Term
removeVar x (Var y) | x==y = Card I
removeVar x (Int i) = Int i
removeVar x (App f y) = app (app (Card S) (removeVar x f)) (removeVar x y)
removeVar x (Lambda y z) = removeVar x (removeVar y z)
removeVar x y = app (Card K) y

app :: Term -> Term -> Term
app (Card K) (Card I) = Card Put
app x y = App x y
