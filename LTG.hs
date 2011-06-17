module LTG where

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as IM

type SlotNum = Int

isValidSlotNum :: Int -> Bool
isValidSlotNum n = 0 <= n && n <= 255

data Value = IntVal Int | PAp Card [Value]
  deriving (Eq, Ord)

instance Show Value where
  show (IntVal n) = show n
  show (PAp c []) = show c
  show (PAp c args) = show c ++ concat (map f args)
    where
      f x = "(" ++ show x ++ ")"

dead, alive :: Int -> Bool
dead v = v == 0 || v == -1
alive = not . dead

data Card
  = I
  | Zero
  | Succ
  | Dbl
  | Get
  | Put
  | S
  | K
  | Inc
  | Dec
  | Attack
  | Help
  | Copy
  | Revive
  | Zombie
  deriving (Eq, Ord, Enum)

arity :: Card -> Int
arity I = 1
arity Zero = 0
arity Succ = 1
arity Dbl = 1
arity Get = 1
arity Put = 2
arity S = 3
arity K = 2
arity Inc = 1
arity Dec = 1
arity Attack = 3
arity Help = 3
arity Copy = 1
arity Revive = 1
arity Zombie = 2

instance Show Card where
  show I      = "I"
  show Zero   = "zero"
  show Succ   = "succ"
  show Dbl    = "dbl"
  show Get    = "get"
  show Put    = "put"
  show S      = "S"
  show K      = "K"
  show Inc    = "inc"
  show Dec    = "dec"
  show Attack = "attack"
  show Help   = "help"
  show Copy   = "copy"
  show Revive = "revive"
  show Zombie = "zombie"

-- (field, vitality)
type PlayerState = (IM.IntMap Value, IM.IntMap Int)

type M = StateT (PlayerState,PlayerState) Maybe

asInt :: Value -> M Int
asInt (IntVal n)  = return n
asInt (PAp Zero []) = return 0
asInt _ = mzero

evalCard :: Card -> M Value
evalCard c
  | arity c == 0 = applyCard c []
  | otherwise    = return $ PAp c []

apply :: Value -> Value -> M Value
apply (IntVal _) _ = mzero
apply (PAp c args) arg
  | arity c == length args + 1 = applyCard c (args++[arg])
  | otherwise = return (PAp c (args ++ [arg]))  

applyCard :: Card -> [Value] -> M Value
applyCard I [val]  = return val
applyCard Zero [] = return (IntVal 0)
applyCard Succ [n] = do
  n <- asInt n
  return $ IntVal $ min (n+1) 0xFFFF
applyCard Dbl [n] = do
  n <- asInt n
  return $ IntVal $ min (n*2) 0xFFFF
applyCard Get [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  return $ f IM.! i
applyCard Put [x,y] = return y
applyCard S [f,g,x] = do
  h <- apply f x
  y <- apply g x
  z <- apply h y
  return z
applyCard K [x,y] = return x
applyCard Inc [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v IM.! i
  when (0 < val && val < 0xFFFF) $
    put ((f, IM.insert i (val+1) v), (f', v'))
  return $ PAp I []
applyCard Dec [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v' IM.! (255-i)
  when (0 < val && val < 0xFFFF) $
    put ((f,v), (f', IM.insert (255-i) (val-1) v'))
  return $ PAp I []
applyCard Attack [i,j,n] = do
  i <- asInt i
  j <- asInt j
  n <- asInt n
  guard $ isValidSlotNum i
  guard $ isValidSlotNum j
  ((f,v),(f',v')) <- get
  let val1 = v IM.! i
      val2 = v' IM.! (255 - j)
      val1' = max 0 (val1 - n)
      val2' = max 0 (val2 - ((n*9) `div` 10))
  put ( (f, IM.insert i val1' v)
      , (f', IM.insert (255 - j) val2' v')
      )
  return $ PAp I []
applyCard Help [i,j,n] = do
  i <- asInt i
  j <- asInt j
  n <- asInt n
  guard $ isValidSlotNum i
  guard $ isValidSlotNum j
  ((f,v),(f',v')) <- get
  let val1 = v IM.! i
      val2 = v IM.! j
      val1' = max 0 (val1 - n)
      val2' = min 0xFFFF (val2 + ((n*11) `div` 10))
  put ((f, IM.insert j val2' $ IM.insert i val1' $ v), (f',v'))
  return $ PAp I []
applyCard Copy [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  return $ f' IM.! i
applyCard Revive [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v IM.! i
  when (val <= 0) $
    put ((f, IM.insert i 1 v), (f',v'))
  return $ PAp I []
applyCard Zombie [i,x] = do
  i <- asInt i
  ((f,v),(f',v')) <- get
  let val = v' IM.! (255-i)
  guard $ dead val
  put ((f,v), (IM.insert (255-i) x f', IM.insert (255-i) (-1) v'))
  return $ PAp I []
applyCard _ _ = mzero

