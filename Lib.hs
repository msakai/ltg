module Lib where

import LTG

copy :: SlotNum -> SlotNum -> [Action]
copy from to = makeNum from to ++ [(L, Get, to)]

makeNum :: Int -> SlotNum -> [Action]
makeNum n to = 
  [ (L, Put, to)  -- toにIが入っていればこれはしなくてOk
  , (R, Zero, to)
  ] ++
  replicate n (L, Succ, to)

apply :: SlotNum -> [Action]
apply res = 
  [(R,Get,res),(L,K,res),(L,S,res),(R,Succ,res),(L,S,res),(R,Get,res),(R,Zero,res)]

compose :: SlotNum -> [Action]
compose c = 
  [(R,K,c),(R,S,c),(L,S,c),(R,K,c)]
