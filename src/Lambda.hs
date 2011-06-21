module Lambda
  ( Var
  , Term (..)
  , (<@>)
  , compile
  , begin
  , beginN
  , let'
  , let_
  ) where

import LTG

import qualified Data.Set as Set

type Var = String

data Term
    = Var String
    | Int !Int
    | Card !Card
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
          error "arity exceeded"
      | otherwise ->
          PAp c (args ++ [toValue y])
toValue (Var v)      = error "should not happen"
toValue (Lambda v a) = error "should not happen"

compile' :: Term -> Term
compile' (App a b)    = app (compile' a) (compile' b)
compile' (Lambda v a) = removeVar v (compile' a)
compile' x            = x

removeVar :: Var -> Term -> Term

-- optimization for special cases
-- removeVar x tm | x `Set.notMember` fvs tm = app (Card K) tm
-- η変換になるから、やっちゃダメ
-- removeVar x (App tm1 (Var y)) | x==y && x `Set.notMember` fvs tm1 = tm1

-- general cases
removeVar x (Var y) | x==y = Card I
removeVar x (App tm1 tm2) = app (app (Card S) (removeVar x tm1)) (removeVar x tm2)
removeVar x tm = app (Card K) tm

app :: Term -> Term -> Term
app (Card I) tm = tm
-- tmの副作用が失われるのでダメ
-- app (Card Put) tm = Card I
app (Card K) (Card I) = Card Put
-- tm3の副作用が複製されるし、副作用の順番が変わるのでダメ
-- app (App (App (Card S) tm1) tm2) tm3 = app (app tm1 tm3) (app tm2 tm3)
app x y = App x y

-- free variables
fvs :: Term -> Set.Set Var
fvs (Var v) = Set.singleton v
fvs (App a b) = fvs a `Set.union` fvs b
fvs (Lambda v a) = Set.delete v (fvs a)
fvs _ = Set.empty

beginFst :: Term -> Term -> Term
beginFst tm1 tm2 = Card K <@> tm1 <@> tm2

beginSnd :: Term -> Term -> Term
beginSnd tm1 tm2 = Card Put <@> tm1 <@> tm2

begin :: [Term] -> Term
begin []  = Card I -- XXX
begin [x] = x
begin [x,y] = beginSnd x y
begin (x:xs) = beginSnd x (begin xs)

beginN :: Int -> [Term] -> Term
beginN _ [] = Card I -- XXX
beginN 0 [x] = x
beginN 0 (x:xs) = beginFst x (begin xs)
beginN n (x:xs) = beginSnd x (beginN (n-1) xs)

-- "let v = tm1 in tm2"
let' :: Var -> Term -> Term -> Term
let' v tm1 tm2 = Lambda v tm2 <@> tm1

-- "let _ = tm1 in tm2"
let_ :: Term -> Term -> Term
let_ tm1 tm2 = beginSnd tm1 tm2
