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
act (flag,c,i) = do
  let a = if flag then rightApply c i else leftApply c i
  s <- get
  let (err,s') = runState a s
  case err of
    Nothing -> return ()
    Just err -> lift $ putStrLn err
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
