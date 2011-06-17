module Lib where

import LTG

move :: SlotNum -> SlotNum -> [Action]
move from to =
  [ (L, Put, to)
  , (R, Zero, to)
  ] ++
  replicate from (L, Succ, to) ++
  [ (L, Get, to) ]
