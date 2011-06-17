module Player where

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import LTG

data LR = L | R deriving (Ord, Eq, Show, Enum, Bounded)

type Action = (LR, Card, SlotNum)

type Player = (Action, Opponent)
newtype Opponent = Opponent (GameState -> Player)

act :: Bool -> Action -> StateT GameState IO ()
act p1 (flag,c,i) = do
  let a = case flag of 
            L -> leftApply c i
            R -> rightApply c i
  s <- get
  let (err,s') =
        if p1
        then runState a s
        else
          case runState a (snd s, fst s) of
            (err,s') -> (err, (snd s', fst s'))
  put s'
  case err of
    Nothing -> return ()
    Just err -> lift $ putStrLn err

play :: Player -> Opponent -> StateT GameState IO ()
play = p1
  where

    p1 :: Player -> Opponent -> StateT GameState IO ()
    p1 p (Opponent op) = do
      lift $ putStrLn "========= player 0"
      get >>= \s -> lift (printState s)
      lift $ print $ (fst p)
      act True (fst p)
      (a,b) <- get
      p2 (op (b,a)) (snd p)

    p2 :: Player -> Opponent -> StateT GameState IO ()
    p2 p (Opponent op) = do
      lift $ putStrLn "========= player 1"
      get >>= \s -> lift (printState s)
      lift $ print $ (fst p)
      act False (fst p)
      s <- get
      p1 (op s) (snd p)

printState (p1,p2) = do
  print $ g p1
  print $ g p2
  where
    g (f,v) = [slot | i <- [0..255], let slot = (i, (v ! i, f ! i))
                    , f ! i /= PAp I [] || v ! i /= 10000]

only :: Player -> StateT GameState IO ()
only p = do
  lift $ putStrLn "========= player 0"
  get >>= \s -> lift (printState s)
  lift $ print $ (fst p)
  act True (fst p)
  s <- get
  case snd p of
    Opponent f -> only (f s)

-- ---------------------------------------------------------------------------

replay :: [Action] -> Player
replay (x:xs) = (x, replay' xs)

replay' :: [Action] -> Opponent
replay' xs = Opponent (\_ -> replay xs)

-- ---------------------------------------------------------------------------

player0 = replay
  [ (R,Zero,0)
  , (L,Succ,0)
  , (L,Succ,0)
  , (L,Dbl,0)
  , (L,Inc,0)
  ]

player1 = replay'
  [ (R, Inc, 0)
  , (R, Zero, 0)
  , (R, Dec, 0)
  , (R, Zero, 2)
  , (L, Succ, 0)
  ]

session1 = runStateT (play player0 player1) initialState

-- ---------------------------------------------------------------------------
