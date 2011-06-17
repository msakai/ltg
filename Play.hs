module Play where

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import LTG

type Player = (Action, Opponent)
newtype Opponent = Opponent (GameState -> Player)

act :: Bool -> Action -> StateT GameState IO ()
act p1 (flag,c,i) = do
  let g = runState $ doAction (flag,c,i)
  s <- get
  let (err,s') =
        if p1
        then g s
        else
          case g (swap s) of
            (err,s') -> (err, (swap s'))
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
      s <- get
      lift $ printState s
      when (-1 `elem` IM.elems (snd (fst s))) $ do
        lift $ putStrLn "Zombie running ..."
        let s' = execState runZombies s
        put s'
        lift $ printState s'
      lift $ print $ (fst p)
      act True (fst p)
      (a,b) <- get
      p2 (op (b,a)) (snd p)

    p2 :: Player -> Opponent -> StateT GameState IO ()
    p2 p (Opponent op) = do
      lift $ putStrLn "========= player 1"
      s <- get
      lift $ printState s
      when (-1 `elem` IM.elems (snd (snd s))) $ do
        lift $ putStrLn "Zombie running ..."
        let s' = swap $ execState runZombies (swap s)
        put s'
        lift $ printState s'
      lift $ print $ (fst p)
      act False (fst p)
      s <- get
      p1 (op s) (snd p)

printState :: GameState -> IO ()
printState (p1,p2) = do
  print $ g p1
  print $ g p2
  where
    g (f,v) = [slot | i <- [0..255], let slot = (i, (v ! i, f ! i))
                    , f ! i /= PAp I [] || v ! i /= 10000]

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

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

player2 = replay
  [ (R, Help, 0)
  , (R, Zero, 0)
  , (L, K,    0)
  , (L, S,    0)
  , (R, Succ, 0)
  , (R, Zero, 0)
  , (R, Zero, 1)
  , (L, Succ, 1)
  , (L, Dbl,  1)
  , (L, Dbl,  1)
  , (L, Dbl,  1)
  , (L, Dbl,  1)
  , (L, K,    0)
  , (L, S,    0)
  , (R, Get,  0)
  , (L, K,    0)
  , (L, S,    0)
  , (R, Succ, 0)
  , (R, Zero, 0)
  ]

session2 = runStateT (only player2) initialState

-- ---------------------------------------------------------------------------

