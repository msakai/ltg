module Eval
  ( Eval
  , runEval
  , evalCard
  , apply
  , doAction
  , runZombies
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Text.Printf

import LTG

-- ---------------------------------------------------------------------------

{-
EvalMonadの中では、手順側のプレイヤーの状態が、GameStateのfstに格納されていると
常に仮定している。
-}
type Eval = ReaderT Bool (ErrorT String (State (GameState, Int)))

runEval :: Bool -> Eval a -> State GameState (Either String a)
runEval zombieMode m = do
  s <- get
  case runState (runErrorT (runReaderT m zombieMode)) (s,0) of
    (ret, (s,_)) -> do
      put s
      return ret

getPlayer0 :: Eval PlayerState
getPlayer0 = do
  ((p0,_),_) <- get
  return p0

getPlayer1 :: Eval PlayerState
getPlayer1 = do
  ((_,p1),_) <- get
  return p1

putPlayer0 :: PlayerState -> Eval ()
putPlayer0 p0 = do
  ((_, p1), apcnt) <- get
  put ((p0, p1), apcnt)

putPlayer1 :: PlayerState -> Eval ()
putPlayer1 p1 = do
  ((p0, _), apcnt) <- get
  put ((p0, p1), apcnt)

countApp :: Eval ()
countApp = do
  (s, apcnt) <- get
  if apcnt >= 1000
    then throwError "number of application > 1000"
    else put (s, apcnt + 1)

asInt :: Value -> Eval Int
asInt (IntVal n)  = return n
asInt (PAp Zero []) = return 0
asInt x = throwError $ show x ++ " is not an integer."

evalCard :: Card -> Eval Value
evalCard c
  | arity c == 0 = applyCard c []
  | otherwise    = return $ PAp c []

apply :: Value -> Value -> Eval Value
apply (IntVal n) _ = throwError $ "cannot apply integer " ++ show n
apply (PAp c args) arg = do
  countApp
  if arity c == length args + 1
    then applyCard c (args++[arg])
    else return (PAp c (args ++ [arg]))  

applyCard :: Card -> [Value] -> Eval Value
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
applyCard Put [x] = return vI
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
  return vI
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
  return $ vI
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
  return vI
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
  return vI
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
  return vI
applyCard Zombie [i,x] = do
  i <- asInt i
  (f',v') <- getPlayer1
  let idx = 255 - i
      val = v' ! idx
  unless (dead val) $ throwError "not dead"
  putPlayer1 (IM.insert idx x f', IM.insert idx (-1) v')
  return vI
applyCard c args = throwError $ "cannot handle " ++ show (PAp c args)

checkValidSlotNum :: SlotNum -> Eval ()
checkValidSlotNum i = 
  unless (isValidSlotNum i) $
    throwError $ show i ++ " is not a valid slot number"

checkAlive :: SlotNum -> Eval ()
checkAlive i = do
  (_,v) <- getPlayer0
  when (dead (v ! i)) $
    throwError $ "slot " ++ show i ++ " is not alive"

-- ---------------------------------------------------------------------------

{-
以下の関数では、先手がGameStateにfstに、後手がGameStateのsndに格納されていると仮定。
現在の手番のプレイヤーがGameStateのfstに入っているとは仮定していないので注意。
引数で現在がisPlayer0によって先手番かどうかを判定。
-}

doAction :: Bool -> Action -> GameState -> (Maybe String, GameState)
doAction isPlayer0 (lr,c,i) s = flip runState s $ do
  unless isPlayer0 swapPlayer
  (f,_) <- gets fst
  ret <- runEval False $ do
    checkAlive i
    c <- evalCard c
    let s = f ! i
    case lr of
      L -> apply c s
      R -> apply s c
  let (val,err) =
        case ret of
          Left err -> (vI, Just err)
          Right val -> (val, Nothing)
  ((f,v),(f',v')) <- get
  put ((IM.insert i val f, v), (f',v'))
  unless isPlayer0 swapPlayer
  return err

runZombies :: Bool -> GameState -> ([String], GameState)
runZombies isPlayer0 s = flip runState s $ do
  unless isPlayer0 swapPlayer
  xs <- liftM concat $ forM [0..255] $ \i -> do
    (f,v) <- gets fst
    if (v ! i == -1)
      then do
        ret <- runEval True $ apply (f ! i) vI
        ((f,v),(f',v')) <- get
        put ((IM.insert i vI f, IM.insert i 0 v), (f',v'))
        case ret of
          Left err -> return [printf "zombie(%d): %s" i err]
          Right _  -> return [printf "zombie(%d)" i]
      else
        return []
  unless isPlayer0 swapPlayer
  return xs

swapPlayer :: State GameState ()
swapPlayer = modify swap
  where
    swap :: (a,b) -> (b,a)
    swap (a,b) = (b,a)

-- ---------------------------------------------------------------------------
