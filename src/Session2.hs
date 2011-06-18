module Session2 where

import Control.Monad.State
import LTG
import Player
import Play

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
