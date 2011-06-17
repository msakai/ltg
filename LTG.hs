module LTG where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Debug.Trace

-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------

-- (field, vitality)
type PlayerState = (IM.IntMap Value, IM.IntMap Int)

initialPlayerState :: PlayerState
initialPlayerState =
  ( IM.fromList [(i, PAp I []) | i <- [0..255]]
  , IM.fromList [(i, 10000) | i <- [0..255]]
  )

type GameState = (PlayerState, PlayerState)

initialState :: GameState
initialState = (initialPlayerState, initialPlayerState)

-- ---------------------------------------------------------------------------

type M = ErrorT String (State GameState)

asInt :: Value -> M Int
asInt (IntVal n)  = return n
asInt (PAp Zero []) = return 0
asInt x = throwError $ show x ++ " is not an integer."

evalCard :: Card -> M Value
evalCard c
  | arity c == 0 = applyCard c []
  | otherwise    = return $ PAp c []

apply :: Value -> Value -> M Value
apply (IntVal n) _ = throwError $ "cannot apply integer " ++ show n
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
  checkValidSlotNum i
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
  checkValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v ! i
  when (0 < val && val < 0xFFFF) $
    put ((f, IM.insert i (val+1) v), (f', v'))
  return $ PAp I []
applyCard Dec [i] = do
  i <- asInt i
  checkValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v' ! (255-i)
  when (0 < val && val < 0xFFFF) $
    put ((f,v), (f', IM.insert (255-i) (val-1) v'))
  return $ PAp I []
applyCard Attack [i,j,n] = do
  i <- asInt i
  j <- asInt j
  n <- asInt n
  checkValidSlotNum i
  checkValidSlotNum j
  ((f,v),(f',v')) <- get
  let val1 = v ! i
      val2 = v' ! (255 - j)
  when (val1 < n) $ throwError "attack error"
  let val1' = val1 - n
      val2' = if dead val2
              then val2
              else max 0 (val2 - ((n*9) `div` 10))
  put ( (f, IM.insert i val1' v)
      , (f', IM.insert (255 - j) val2' v')
      )
  return $ PAp I []
applyCard Help [i,j,n] = do
  i <- asInt i
  j <- asInt j
  n <- asInt n
  checkValidSlotNum i
  checkValidSlotNum j
  ((f,v),(f',v')) <- get
  let v2 = IM.insert i (max 0 ((v ! i) - n)) v
      v3 = IM.insert j (min 0xFFFF ((v2 ! j) + ((n*11) `div` 10))) v2
  put ((f,v3), (f',v'))
  return $ PAp I []
applyCard Copy [i] = do
  i <- asInt i
  checkValidSlotNum i
  ((f,v),(f',v')) <- get
  return $ f' ! i
applyCard Revive [i] = do
  i <- asInt i
  checkValidSlotNum i
  ((f,v),(f',v')) <- get
  let val = v ! i
  when (val <= 0) $
    put ((f, IM.insert i 1 v), (f',v'))
  return $ PAp I []
applyCard Zombie [i,x] = do
  i <- asInt i
  ((f,v),(f',v')) <- get
  let val = v' ! (255-i)
  unless (dead val) $ throwError "not dead"
  put ((f,v), (IM.insert (255-i) x f', IM.insert (255-i) (-1) v'))
  return $ PAp I []
applyCard c args = throwError $ "cannot handle " ++ show (PAp c args)

checkValidSlotNum :: SlotNum -> M ()
checkValidSlotNum i = 
  unless (isValidSlotNum i) $
    throwError $ show i ++ " is not a valid slot number"

checkAlive :: SlotNum -> M ()
checkAlive i = do
  ((_,v),_) <- get
  when (dead (v ! i)) $
    throwError $ "slot " ++ show i ++ " is not alive"

-- ---------------------------------------------------------------------------

data LR = L | R deriving (Ord, Eq, Show, Enum, Bounded)
type Action = (LR, Card, SlotNum)

type M2 = State GameState

doAction :: Action -> M2 (Maybe String)
doAction (lr,c,i) = do
  ((f,_),_) <- get
  ret <- runErrorT $ do
    checkAlive i
    c <- evalCard c    
    case lr of
      L -> apply c (f ! i)
      R -> apply (f ! i) c        
  let (val,err) =
        case ret of
          Left err -> (PAp I [], Just err)
          Right val -> (val, Nothing)
  ((f,v),(f',v')) <- get
  put $ ((IM.insert i val f, v), (f',v'))
  return err

changeTurn :: M2 ()
changeTurn = do
  (proponent, opponent) <- get
  put (opponent, proponent)

traceState :: M2 ()
traceState = do
  (proponent, opponent) <- get
  let g (f,v) = [slot | i <- [0..255], let slot = (i, (v ! i, f ! i))
                      , f ! i /= PAp I [] || v ! i /= 10000]
  trace "=========" $ return ()
  trace (show (g proponent)) $ return ()
  trace (show (g opponent)) $ return ()

-- ---------------------------------------------------------------------------

test = flip runState initialState $ do
  -- proponent
  doAction (R, Zero, 0)
  traceState
  changeTurn

  -- opponent
  doAction (R, Inc, 0)
  changeTurn
  traceState

  -- proponent
  doAction (L, Succ, 0)
  traceState
  changeTurn

  -- opponent
  doAction (R, Zero, 0)
  changeTurn
  traceState

  -- proponent
  doAction (L, Succ, 0)
  traceState
  changeTurn

  -- opponent
  doAction (R, Dec, 0)
  changeTurn
  traceState

  -- proponent
  doAction (L, Dbl, 0)
  traceState
  changeTurn

  -- opponent
  doAction (R, Zero, 2)
  changeTurn
  traceState

  -- proponent
  doAction (L, Inc, 0)
  traceState
  changeTurn

  -- opponent
  doAction (L, Succ, 0)
  changeTurn
  traceState

-- ---------------------------------------------------------------------------
