module Play where

import Control.Monad
import Control.Monad.State
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import System.Environment (getArgs)
import LTG

-- Player is a (infinite state) Mealy Machine
-- input is GameState
-- output is Action
newtype Player = Player{ trans :: GameState -> (Action, Player) }

act :: Bool -> Action -> StateT GameState IO ()
act isPlayer0 action = do
  err <- doAction isPlayer0 action
  case err of
    Nothing -> return ()
    Just err -> lift $ putStrLn err

zom :: Bool -> StateT GameState IO ()
zom isPlayer0 = do
  s <- get
  let s2 = if isPlayer0 then fst s else snd s
  when (-1 `elem` IM.elems (snd s2)) $ do
    lift $ putStrLn "Zombie running ..."
    msgs <- runZombies isPlayer0
    lift $ mapM_ putStrLn msgs
    s <- get
    lift $ printState s

play :: Player -> Player -> StateT GameState IO ()
play = go True
  where
    go :: Bool -> Player -> Player -> StateT GameState IO ()
    go isPlayer0 p0 p1 = do
      lift $ putStrLn $ "========= player " ++ (if isPlayer0 then "0" else "1")
      s <- get
      lift $ printState s
      zom isPlayer0
      s <- get
      if isPlayer0
        then do
          let (action,p0') = trans p0 s
          lift $ print action
          act isPlayer0 action
          go False p0' p1 
        else do
          let (action, p1') = trans p1 s
          lift $ print action      
          act isPlayer0 action
          go True p0 p1'

printState :: GameState -> IO ()
printState (p1,p2) = do
  print $ g p1
  print $ g p2
  where
    g (f,v) = [slot | i <- [0..255], let slot = (i, (v ! i, f ! i))
                    , f ! i /= PAp I [] || v ! i /= 10000]

only :: Player -> StateT GameState IO ()
only p = do
  lift $ putStrLn "========= player 0"
  s <- get
  lift $ printState s
  let (action,p') = trans p s
  lift $ print action
  act True action
  only p'

replay :: [Action] -> Player
replay (x:xs) = Player (\_ -> (x, replay xs))

-- ---------------------------------------------------------------------------

-- runコマンドの実装用
runPlayer :: Player -> IO ()
runPlayer p = flip evalStateT initialState $ do
  args <- lift getArgs
  let isPlayer0 = (args /= ["1"])

      opp :: Player -> StateT GameState IO ()
      opp p = do
        lr <- lift getLine
        case lr of
          "1" -> do
            card <- lift $ liftM cardOfName getLine
            slot <- lift $ readLn
            act (not isPlayer0) (L, card, slot)
          "2" -> do
            slot <- lift $ readLn
            card <- lift $ liftM cardOfName getLine
            act (not isPlayer0) (R, card, slot)
        prop p

      prop :: Player -> StateT GameState IO ()
      prop p = do
        s <- get
        let ((lr,card,slot), p') = trans p s
        case lr of
          L -> do
            lift $ print 1
            lift $ putStrLn $ cardName card
            lift $ print slot
          R -> do
            lift $ putStrLn "2"
            lift $ print slot
            lift $ putStrLn $ cardName card
        opp p'
  if isPlayer0
    then opp p
    else prop p
