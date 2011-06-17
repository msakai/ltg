module LTG where

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as IM
import Data.IntMap ((!))

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

initialPlayerState :: PlayerState
initialPlayerState =
  ( IM.fromList [(i, PAp I []) | i <- [0..255]]
  , IM.fromList [(i, 10000) | i <- [0..255]]
  )

initialState :: (PlayerState, PlayerState)
initialState = (initialPlayerState, initialPlayerState)

type Error = String

type M = StateT (PlayerState,PlayerState) (Either Error)

runM :: StateT (PlayerState, PlayerState) m a
     -> m (a, (PlayerState, PlayerState))
runM m = runStateT m initialState

asInt :: Value -> M Int
asInt (IntVal n)  = return n
asInt (PAp Zero []) = return 0
asInt x = lift $ Left $ show x ++ "is not an integer."

evalCard :: Card -> M Value
evalCard c
  | arity c == 0 = applyCard c []
  | otherwise    = return $ PAp c []

apply :: Value -> Value -> M Value
apply (IntVal n) _ = lift $ Left $ "cannot apply integer " ++ show n
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
  return $ f ! i
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
  let val = v ! i
  when (0 < val && val < 0xFFFF) $
    put ((f, IM.insert i (val+1) v), (f', v'))
  return $ PAp I []
applyCard Dec [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v' ! (255-i)
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
  let val1 = v ! i
      val2 = v' ! (255 - j)
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
  let v2 = IM.insert i (max 0 ((v ! i) - n)) v
      v3 = IM.insert j (min 0xFFFF ((v2 ! j) + ((n*11) `div` 10))) v2
  put ((f,v3), (f',v'))
  return $ PAp I []
applyCard Copy [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  return $ f' ! i
applyCard Revive [i] = do
  i <- asInt i
  guard $ isValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v ! i
  when (val <= 0) $
    put ((f, IM.insert i 1 v), (f',v'))
  return $ PAp I []
applyCard Zombie [i,x] = do
  i <- asInt i
  ((f,v),(f',v')) <- get
  let val = v' ! (255-i)
  guard $ dead val
  put ((f,v), (IM.insert (255-i) x f', IM.insert (255-i) (-1) v'))
  return $ PAp I []
applyCard c args = lift $ Left $ "cannot handle " ++ show (PAp c args)

changeTurn :: M ()
changeTurn = do
  (proponent, opponent) <- get
  put (opponent, proponent)

