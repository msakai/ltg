module Lambda
  ( Var
  , Term (..)
  , (<@>)
  , compile
  ) where

import LTG

import qualified Data.Set as Set

type Var = String

data Term
    = Var String
    | Int !Int
    | Card Card
    | App Term Term
    | Lambda Var Term
    deriving Show

infixl 1 <@>

(<@>) :: Term -> Term -> Term
(<@>) = App

compile :: Term -> Value
compile = toValue . compile'

toValue :: Term -> Value
toValue (Int i)   = IntVal i
toValue (Card c)  = PAp c []
toValue (App x y) =
  case toValue x of
    IntVal _ -> error "cannot apply integer value"
    PAp c args
      | length args + 1 >= arity c ->
          error "arity exceeded" -- FIXME: arityˆÈã‚É‚È‚é‚Æ‚«‚É‚Ç‚¤‚·‚é?
      | otherwise ->
          PAp c (args ++ [toValue y])
toValue (Var v)      = error "should not happen"
toValue (Lambda v a) = error "should not happen"

compile' :: Term -> Term
compile' (App a b)    = app (compile' a) (compile' b)
compile' (Lambda v a) = removeVar v (compile' a)
compile' x            = x

removeVar :: Var -> Term -> Term
removeVar x (Var y) | x==y = Card I
removeVar x tm | x `Set.notMember` fvs tm = app (Card K) tm
removeVar x (App f y) = app (app (Card S) (removeVar x f)) (removeVar x y)
removeVar x tm = app (Card K) tm

app :: Term -> Term -> Term
app (Card I) tm = tm
-- app (Card Put) tm = Card I -- tm‚Ì•›ì—p‚ªŽ¸‚í‚ê‚é‚Ì‚Åƒ_ƒ
app (Card K) (Card I) = Card Put
app (App (App (Card S) tm1) tm2) tm3 = app (app tm1 tm3) (app tm2 tm3)
app x y = App x y

-- free variables
fvs :: Term -> Set.Set Var
fvs (Var v) = Set.singleton v
fvs (App a b) = fvs a `Set.union` fvs b
fvs (Lambda v a) = Set.delete v (fvs a)
fvs _ = Set.empty
