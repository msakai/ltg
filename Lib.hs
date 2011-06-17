module Lib where

import LTG

copy :: SlotNum -> SlotNum -> [Action]
copy from to = makeNum from to ++ [(L, Get, to)]

makeNum :: Int -> SlotNum -> [Action]
makeNum n to = 
  [ (L, Put, to)
  , (R, Zero, to)
  ] ++
  replicate n (L, Succ, to)

apply :: SlotNum -> [Action]
apply res = 
  [(R,Get,res),(L,K,res),(L,S,res),(R,Succ,res),(L,S,res),(R,Get,res),(R,Zero,res)]
