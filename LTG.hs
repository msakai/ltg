module LTG where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
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
arity Put = 1
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
{-
このファイルの関数では、常に手順側のプレイヤーの状態がfstに格納されている
と仮定している。
-}

initialState :: GameState
initialState = (initialPlayerState, initialPlayerState)

-- ---------------------------------------------------------------------------

type M = ReaderT Bool (ErrorT String (State GameState))

runM :: Bool -> M a -> State GameState (Either String a)
runM zombieMode m = runErrorT (runReaderT m zombieMode)

getPlayer0 :: M PlayerState
getPlayer0 = do
  (p0,_) <- get
  return p0

getPlayer1 :: M PlayerState
getPlayer1 = do
  (_,p1) <- get
  return p1

putPlayer0 :: PlayerState -> M ()
putPlayer0 p0 = do
  (_, p1) <- get
  put (p0, p1)  

putPlayer1 :: PlayerState -> M ()
putPlayer1 p1 = do
  (p0, _) <- get
  put (p0, p1)

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
  (f,v) <- getPlayer0
  return $ f ! i
applyCard Put [x] = return (PAp I [])
applyCard S [f,g,x] = do
  h <- apply f x
  y <- apply g x
  z <- apply h y
  return z
applyCard K [x,y] = return x
applyCard Inc [i] = do
  i <- asInt i
  checkValidSlotNum i
  (f,v) <- getPlayer0
  let val = v ! i
  zombieMode <- ask
  if not zombieMode
    then
      when (0 < val && val < 0xFFFF) $
        putPlayer0 (f, IM.insert i (val+1) v)
    else
      when (0 < val) $
        putPlayer0 (f, IM.insert i (val-1) v)
  return $ PAp I []
applyCard Dec [i] = do
  i <- asInt i
  checkValidSlotNum i
  (f',v') <- getPlayer1
  let idx = 255 - i
      val = v' ! idx
  zombieMode <- ask
  if not zombieMode
    then
      when (0 < val) $
        putPlayer1 (f', IM.insert idx (val-1) v')
    else
      when (0 < val && val < 0xFFFF) $
        putPlayer1 (f', IM.insert idx (val+1) v')
  return $ PAp I []
applyCard Attack [i,j,n] = do
  i <- asInt i
  j <- asInt j
  n <- asInt n
  checkValidSlotNum i
  checkValidSlotNum j
  (f,v)   <- getPlayer0
  (f',v') <- getPlayer1
  let val1 = v ! i
      idx2 = 255 - j
      val2 = v' ! idx2
  zombieMode <- ask
  if not zombieMode
    then do
      when (val1 < n) $ throwError "attack error"
      let val1' = val1 - n
          val2' = if dead val2
                  then val2
                  else max 0 (val2 - ((n*9) `div` 10))
      putPlayer0 (f,  IM.insert i val1' v)
      putPlayer1 (f', IM.insert idx2 val2' v')
    else do
      let val2' = min 0xFF (val2 + ((n*9) `div` 10))
      putPlayer1 (f', IM.insert idx2 val2' v')
  return $ PAp I []
applyCard Help [i,j,n] = do
  i <- asInt i
  j <- asInt j
  n <- asInt n
  checkValidSlotNum i
  checkValidSlotNum j
  (f,v) <- getPlayer0
  zombieMode <- ask
  if not zombieMode
    then do
      let val1 = v ! i
      when (val1 < n) $ throwError "help error"
      let v2 = IM.insert i (val1 - n) v
          v3 = IM.insert j (min 0xFFFF ((v2 ! j) + ((n*11) `div` 10))) v2
      putPlayer0 (f,v3)
    else do
      let v2 = IM.insert j (max 0 (((v ! j) - ((n*11) `div` 10)))) v
      putPlayer0 (f,v2)
  return $ PAp I []
applyCard Copy [i] = do
  i <- asInt i
  checkValidSlotNum i
  (f',_) <- getPlayer1
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
  (f',v') <- getPlayer1
  let idx = 255 - i
      val = v' ! idx
  unless (dead val) $ throwError "not dead"
  putPlayer1 (IM.insert idx x f', IM.insert idx (-1) v')
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
  ret <- runM False $ do
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

runZombies :: M2 [String]
runZombies = do
  liftM concat $ forM [0..255] $ \i -> do
    ((f,v),_) <- get
    if (v ! i == -1)
      then do
        ret <- runM True $ apply (f ! i) (PAp I [])
        ((f,v),(f',v')) <- get
        put ((IM.insert i (PAp I []) f, IM.insert i 0 v), (f',v'))
        case ret of
          Left err -> return [err]
          Right _ -> return []
      else
        return []

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
  runZombies
  doAction (R, Zero, 0)
  traceState
  changeTurn

  -- opponent
  runZombies
  doAction (R, Inc, 0)
  changeTurn
  traceState

  -- proponent
  runZombies
  doAction (L, Succ, 0)
  traceState
  changeTurn

  -- opponent
  runZombies
  doAction (R, Zero, 0)
  changeTurn
  traceState

  -- proponent
  runZombies
  doAction (L, Succ, 0)
  traceState
  changeTurn

  -- opponent
  runZombies
  doAction (R, Dec, 0)
  changeTurn
  traceState

  -- proponent
  runZombies
  doAction (L, Dbl, 0)
  traceState
  changeTurn

  -- opponent
  runZombies
  doAction (R, Zero, 0)
  changeTurn
  traceState

  -- proponent
  runZombies
  doAction (L, Inc, 0)
  traceState
  changeTurn

  -- opponent
  runZombies
  doAction (L, Succ, 0)
  changeTurn
  traceState

-- ---------------------------------------------------------------------------
