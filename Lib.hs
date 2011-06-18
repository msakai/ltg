module Lib where

import LTG
import SlotName

copy,copy' :: SlotNum -> SlotNum -> [Action]
copy  from to = makeNum  from to ++ [(L, Get, to)]
copy' from to = makeNum' from to ++ [(L, Get, to)]

makeNum, makeNum' :: Int -> SlotNum -> [Action]
makeNum n to = 
  [ (R, Zero, to)
  ] ++
  replicate n (L, Succ, to)
makeNum' n to = 
  [ (L, Put, to)  -- toにIが入っていればこれはしなくてOk
  , (R, Zero, to)
  ] ++
  replicate n (L, Succ, to)

apply,apply' :: SlotNum -> [Action]
apply res = 
  [(R,Get,res)
  ,(L,K,res)
  ,(L,S,res)
  ,(R,Succ,res)
  ,(L,S,res)
  ,(R,Get,res)
  ,(R,Zero,res)
  ]
apply' res = 
  [(L,Put,res)  -- resにIが入っていればこれはしなくてOk
  ,(R,Get,res)
  ,(L,K,res)
  ,(L,S,res)
  ,(R,Succ,res)
  ,(L,S,res)
  ,(R,Get,res)
  ,(R,Zero,res)
  ]

compose, compose' :: SlotNum -> [Action]
compose compo = 
  [(R,K,compo)
  ,(R,S,compo)
  ,(L,S,compo)
  ,(R,K,compo)
  ]
compose' compo = 
  [(L,Put,compo) -- compoにIが入っていればこれはしなくてOk
  ,(R,K,compo)
  ,(R,S,compo)
  ,(L,S,compo)
  ,(R,K,compo)
  ]

mocking,mocking' :: SlotNum -> [Action] -- M
mocking to =
  [ (L, S  , to)
  , (R, I  , to) 
  ]
mocking' to =
  [ (L, Put, to)  -- toにIが入っていればこれはしなくてOk
  , (L, S  , to)
  , (R, I  , to) 
  ]

makeRadices,makeRadices' :: [Action] -- 2^0〜2^15 を所定のスロットに格納
makeRadices =
  [(R,Zero,0)                   -- s[0]   = 0
  ,(L,Succ,0)                   -- s[0]  += 1
  ,(R,Get,_b0),(R,Zero,_b0)     -- s[_b0] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b1),(R,Zero,_b1)     -- s[_b1] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b2),(R,Zero,_b2)     -- s[_b2] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b3),(R,Zero,_b3)     -- s[_b3] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b4),(R,Zero,_b4)     -- s[_b4] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b5),(R,Zero,_b5)     -- s[_b5] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b6),(R,Zero,_b6)     -- s[_b6] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b7),(R,Zero,_b7)     -- s[_b7] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b8),(R,Zero,_b8)     -- s[_b8] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b9),(R,Zero,_b9)     -- s[_b9] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b10),(R,Zero,_b10)   -- s[_b10] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b11),(R,Zero,_b11)   -- s[_b11] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b12),(R,Zero,_b12)   -- s[_b12] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b13),(R,Zero,_b13)   -- s[_b13] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b14),(R,Zero,_b14)   -- s[_b14] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b15),(R,Zero,_b15)   -- s[_b15] = s[0]
  ]
makeRadices' =
  [(L,Put,0)
  ,(R,Zero,0)                   -- s[0]   = 0
  ,(L,Succ,0)                   -- s[0]  += 1
  ,(R,Get,_b0),(R,Zero,_b0)     -- s[_b0] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b1),(R,Zero,_b1)     -- s[_b1] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b2),(R,Zero,_b2)     -- s[_b2] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b3),(R,Zero,_b3)     -- s[_b3] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b4),(R,Zero,_b4)     -- s[_b4] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b5),(R,Zero,_b5)     -- s[_b5] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b6),(R,Zero,_b6)     -- s[_b6] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b7),(R,Zero,_b7)     -- s[_b7] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b8),(R,Zero,_b8)     -- s[_b8] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b9),(R,Zero,_b9)     -- s[_b9] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b10),(R,Zero,_b10)   -- s[_b10] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b11),(R,Zero,_b11)   -- s[_b11] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b12),(R,Zero,_b12)   -- s[_b12] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b13),(R,Zero,_b13)   -- s[_b13] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b14),(R,Zero,_b14)   -- s[_b14] = s[0]
  ,(L,Dbl,0)                    -- s[0]   *= 2
  ,(R,Get,_b15),(R,Zero,_b15)   -- s[_b15] = s[0]
  ]

--------------------------------------------------------------------------------
-- initComb     : combの割り当てられたスロット(Iが入っている)をcombに初期化
-- overloadComb : combの割り当てられたスロット(I以外が入っている)をcombで上書き
-- initComb'    : 指定したスロットを(Iが入っている)をcombに初期化
-- overloadComb': 指定したスロット(I以外が入っている)をcombで上書き

initMakeNumNaive, overloadMakeNumNaive :: Int -> [Action] -- 素朴版
initMakeNumNaive n     = (R, Zero, _makeNum) : replicate n (L, Succ, _makeNum)
overloadMakeNumNaive n = (L, Put, _makeNum)  : initMakeNumNaive n

