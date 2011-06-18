module Player
  ( Player (..)
  , replay
  ) where

import LTG

{-
Player is a (infinite state) Mealy Machine
input: GameState
output: Action
GameStateは(自分,相手)の順番
-}
newtype Player = Player{ trans :: GameState -> (Action, Player) }

replay :: [Action] -> Player
replay (x:xs) = Player (\_ -> (x, replay xs))
