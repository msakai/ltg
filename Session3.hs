module Session3 where

import Control.Monad.State
import LTG
import Play

-- Infinite Loop example.
player3 = replay
  [ (R, S,    0)
  , (R, Get,  0)
  , (R, I,    0)
  , (R, Zero, 0)
  ]

session3 = runStateT (only player3) initialState
