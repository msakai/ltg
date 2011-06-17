module Player where

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import LTG

data LR = L | R deriving (Ord, Eq, Show, Enum, Bounded)

-- False = left application
-- True  = right application
type Action = (LR, Card, SlotNum)

type Player = (Action, Opponent)
newtype Opponent = Opponent (GameState -> Player)

act :: Action -> StateT GameState IO ()
act (flag,c,i) = do
  let a = case flag of 
            L -> leftApply c i
            R -> rightApply c i
  s <- get
  let (err,s') = runState a s
  case err of
    Nothing -> return ()
    Just err -> lift $ putStrLn err
  put s'

play :: Player -> Opponent -> StateT GameState IO ()
play = p1
  where
    g (f,v) = [slot | i <- [0..255], let slot = (i, (v ! i, f ! i))
                    , f ! i /= PAp I [] || v ! i /= 10000]
    printState (p1,p2) = do
      print $ g p1
      print $ g p2

    p1 :: Player -> Opponent -> StateT GameState IO ()
    p1 p (Opponent op) = do
      lift $ putStrLn "========= player 1"
      get >>= \s -> lift (printState s)
      act (fst p)
      (a,b) <- get
      p2 (op (b,a)) (snd p)

    p2 :: Player -> Opponent -> StateT GameState IO ()
    p2 p (Opponent op) = do
      lift $ putStrLn "========= player 2"
      get >>= \s -> lift (printState s)
      act (fst p)
      s <- get
      p1 (op s) (snd p)
