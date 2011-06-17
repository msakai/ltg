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
  [(L,Put,res)  -- resにIが入っていればこれはしなくてOk
  ,(R,Get,res)
  ,(L,K,res)
  ,(L,S,res)
  ,(R,Succ,res)
  ,(L,S,res)
  ,(R,Get,res)
  ,(R,Zero,res)
  ]

compose :: SlotNum -> [Action]
compose compo = 
  [(L,Put,compo) -- compoにIが入っていればこれはしなくてOk
  ,(R,K,compo)
  ,(R,S,compo)
  ,(L,S,compo)
  ,(R,K,compo)
  ]

mocking :: SlotNum -> [Action] -- M
mocking to =
  [ (L, Put, to)  -- toにIが入っていればこれはしなくてOk
  , (L, S  , to)
  , (R, I  , to) 
  ]
