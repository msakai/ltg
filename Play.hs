module Player where

import Control.Monad
import Control.Monad.State
import LTG

-- False = left application
-- True  = right application
type Action = (Bool, Card, SlotNum)

type Player = (Action, Opponent)
newtype Opponent = Opponent (GameState -> Player)

act :: Action -> StateT GameState IO ()
act (False,c,i) = do
  s <- get
  let (_,s') = runState (leftApply c i) s
  put s'
act (True,c,i)  = do
  s <- get
  let (_,s') = runState (rightApply c i) s
  put s'

play :: Player -> Opponent -> StateT GameState IO ()
play = p1
  where
    p1 :: Player -> Opponent -> StateT GameState IO ()
    p1 p (Opponent op) = do
      act (fst p)
      (a,b) <- get
      p2 (op (b,a)) (snd p)

    p2 :: Player -> Opponent -> StateT GameState IO ()
    p2 p (Opponent op) = do
      act (fst p)
      s <- get
      p1 (op s) (snd p)
