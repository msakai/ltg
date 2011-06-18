module Session1 where

import Control.Monad.State
import LTG
import Player
import Play (play)

player0 = replay
  [ (R,Zero,0)
  , (L,Succ,0)
  , (L,Succ,0)
  , (L,Dbl,0)
  , (L,Inc,0)
  ]

player1 = replay
  [ (R, Inc, 0)
  , (R, Zero, 0)
  , (R, Dec, 0)
  , (R, Zero, 0)
  , (L, Succ, 0)
  ]

session1 = runStateT (play player0 player1) initialState
