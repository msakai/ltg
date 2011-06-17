module Play where

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import LTG

-- Player is a (infinite state) Mealy Machine
-- input is GameState
-- output is Action
newtype Player = Player{ trans :: GameState -> (Action, Player) }

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

play :: Player -> Player -> StateT GameState IO ()
play = turnA
  where

    turnA :: Player -> Player -> StateT GameState IO ()
    turnA p0 p1 = do
      lift $ putStrLn "========= player 0"
      s <- get
      lift $ printState s
      when (-1 `elem` IM.elems (snd (fst s))) $ do
        lift $ putStrLn "Zombie running ..."
        let s' = execState runZombies s
        put s'
        lift $ printState s'
      s <- get
      let (action,p0') = trans p0 s
      lift $ print $ action
      act True action
      turnB p0 p1 

    turnB :: Player -> Player -> StateT GameState IO ()
    turnB p0 p1 = do
      lift $ putStrLn "========= player 1"
      s <- get
      lift $ printState s
      when (-1 `elem` IM.elems (snd (snd s))) $ do
        lift $ putStrLn "Zombie running ..."
        let s' = swap $ execState runZombies (swap s)
        put s'
        lift $ printState s'
      s <- get
      let (action, p1') = trans p1 s
      lift $ print action      
      act False action
      turnA p0 p1'

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
  s <- get
  lift $ printState s
  let (action,p') = trans p s
  lift $ print action
  act True action
  only p'

-- ---------------------------------------------------------------------------

replay :: [Action] -> Player
replay (x:xs) = Player (\_ -> (x, replay xs))

-- obsoleted
replay' :: [Action] -> Player
replay' = replay

-- ---------------------------------------------------------------------------
