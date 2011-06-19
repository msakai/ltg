module Player
  ( module LTG

  , Player (..)
  , replay

  , Task (..)
  , runTask'
  , getState
  , execAction
  ) where

import LTG

-- ---------------------------------------------------------------------------

{-
Player is a (infinite state) Mealy Machine
input: GameState
output: Action
GameStateは(自分,相手)の順番
-}
newtype Player = Player{ trans :: GameState -> (Action, Player) }

replay :: [Action] -> Player
replay (x:xs) = Player (\_ -> (x, replay xs))
-- replay xs = runTask' (mapM_ execAction xs)

-- ---------------------------------------------------------------------------

-- kind of a CPS monad
newtype Task a = Task{ runTask :: (a -> Player) -> Player }

runTask' :: Task a -> Player
runTask' m = runTask m (error "runTask'")

instance Monad Task where
  return a = Task $ \k -> k a
  m >>= f = Task $ \k -> runTask m $ \a -> runTask (f a) k

getState :: Task GameState
getState = Task $ \k -> Player $ \s -> trans (k s) s

execAction :: Action -> Task ()
execAction a = Task $ \k -> Player $ \s -> (a, k ())

-- ---------------------------------------------------------------------------
