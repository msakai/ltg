module LTG where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Text.Printf

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
  deriving (Eq, Ord, Enum,Show,Read)

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

cardName :: Card -> String
cardName I      = "I"
cardName Zero   = "zero"
cardName Succ   = "succ"
cardName Dbl    = "dbl"
cardName Get    = "get"
cardName Put    = "put"
cardName S      = "S"
cardName K      = "K"
cardName Inc    = "inc"
cardName Dec    = "dec"
cardName Attack = "attack"
cardName Help   = "help"
cardName Copy   = "copy"
cardName Revive = "revive"
cardName Zombie = "zombie"

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

type M = ReaderT Bool (ErrorT String (State (GameState, Int)))

runM :: Bool -> M a -> State GameState (Either String a)
runM zombieMode m = do
  s <- get
  case runState (runErrorT (runReaderT m zombieMode)) (s,0) of
    (ret, (s,_)) -> do
      put s
      return ret

getPlayer0 :: M PlayerState
getPlayer0 = do
  ((p0,_),_) <- get
  return p0

getPlayer1 :: M PlayerState
getPlayer1 = do
  ((_,p1),_) <- get
  return p1

putPlayer0 :: PlayerState -> M ()
putPlayer0 p0 = do
  ((_, p1), apcnt) <- get
  put ((p0, p1), apcnt)

putPlayer1 :: PlayerState -> M ()
putPlayer1 p1 = do
  ((p0, _), apcnt) <- get
  put ((p0, p1), apcnt)

countApp :: M ()
countApp = do
  (s, apcnt) <- get
  if apcnt >= 1000
    then throwError "number of application > 1000"
    else put (s, apcnt + 1)

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
apply (PAp c args) arg = do
  countApp
  if arity c == length args + 1
    then applyCard c (args++[arg])
    else return (PAp c (args ++ [arg]))  

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
  (f,v) <- getPlayer0
  let val = v ! i
  when (val <= 0) $
    putPlayer0 (f, IM.insert i 1 v)
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
  (_,v) <- getPlayer0
  when (dead (v ! i)) $
    throwError $ "slot " ++ show i ++ " is not alive"

-- ---------------------------------------------------------------------------

data LR = L | R deriving (Ord, Eq, Show, Enum, Bounded)
type Action = (LR, Card, SlotNum)

doAction :: Action -> State GameState (Maybe String)
doAction (lr,c,i) = do
  (f,_) <- gets fst
  ret <- runM False $ do
    checkAlive i
    c <- evalCard c
    let s = f ! i
    case lr of
      L -> apply c s
      R -> apply s c
  let (val,err) =
        case ret of
          Left err -> (PAp I [], Just err)
          Right val -> (val, Nothing)
  ((f,v),(f',v')) <- get
  put ((IM.insert i val f, v), (f',v'))
  return err

runZombies :: State GameState [String]
runZombies = do
  liftM concat $ forM [0..255] $ \i -> do
    (f,v) <- gets fst
    if (v ! i == -1)
      then do
        ret <- runM True $ apply (f ! i) (PAp I [])
        ((f,v),(f',v')) <- get
        put ((IM.insert i (PAp I []) f, IM.insert i 0 v), (f',v'))
        case ret of
          Left err -> return [printf "zombie(%d): %s" i err]
          Right _  -> return [printf "zombie(%d)" i]
      else
        return []

-- ---------------------------------------------------------------------------
