module Lib where

import LTG
import SlotName
import Control.Monad.Writer
import Control.Monad.State
import qualified Data.IntSet as IS

copy,copy' :: SlotNum -> SlotNum -> [Action]
copy  from to = makeNum  from to ++ [(L, Get, to)]
copy' from to = overloading to $ copy from to

makeNum, makeNum' :: Int -> SlotNum -> [Action]
makeNum n to = 
  [ (R, Zero, to)
  ] ++
  replicate n (L, Succ, to)
makeNum' n to = overloading to $ makeNum n to

-- apply を設定したスロットにZeroを右適用すると，
-- スロット1に置かれた関数をスロット0 に置かれた
-- 引数に適用してその結果が指定したスロットresに
-- 格納される．
apply,apply' :: SlotNum -> [Action] 
apply res = 
  [(R,Get,res)
  ,(L,K,res)
  ,(L,S,res)
  ,(R,Succ,res)
  ,(L,S,res)
  ,(R,Get,res)
  ]
apply' res = overloading res $ apply res

doApply :: SlotNum -> [Action]
doApply res = [(R,Zero,res)] -- resにはapplyが格納されていることを仮定する

compose, compose' :: SlotNum -> [Action]
compose compo = 
  [(R,K,compo)
  ,(R,S,compo)
  ,(L,S,compo)
  ,(R,K,compo)
  ]
compose' compo = overloading compo $ compose compo

mocking,mocking' :: SlotNum -> [Action] -- M
mocking to =
  [ (L, S  , to)
  , (R, I  , to) 
  ]
mocking' to = overloading to $ mocking to

makeRadices,makeRadices' :: [Action] -- 2^0〜2^15 を所定のスロットに格納
makeRadices =
  [(R,Zero,_arg)                -- s[0]   = 0
  ,(L,Succ,_arg)                -- s[0]  += 1
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
makeRadices' = overloading _arg $ makeRadices

--------------------------------------------------------------------------------
-- initComb     : combの割り当てられたスロット(Iが入っている)をcombに初期化
-- overloadComb : combの割り当てられたスロット(I以外が入っている)をcombで上書き
-- initComb'    : 指定したスロットを(Iが入っている)をcombに初期化
-- overloadComb': 指定したスロット(I以外が入っている)をcombで上書き

initMakeNumNaive, overloadMakeNumNaive :: Int -> [Action] -- 素朴版
initMakeNumNaive n     = (R, Zero, _makeNum) : replicate n (L, Succ, _makeNum)
overloadMakeNumNaive n = overloading n $ initMakeNumNaive n

overloading :: SlotNum -> ([Action] -> [Action])
overloading s = ((L,Put,s):)

-- apply
initApply,overloadApply :: [Action]
initApply     = apply  _apply
overloadApply = apply' _apply

initApply',overloadApply' :: SlotNum -> [Action]
initApply'     = apply
overloadApply' = apply'

setArg, setFun :: SlotNum -> [Action]
setArg from = copy' from _arg
setFun from = copy' from _fun
setRes :: [Action]
setRes      = copy' _apply _res

-- (B)lue bird: function composition
initB, overloadB :: [Action]
initB = initB' _B
overloadB = overloading _B $ initB

initB', overloadB' :: SlotNum -> [Action]
initB' s = [(R,K,s),(R,S,s)  -- (KS)
           ,(L,S,s)          -- S(KS)
           ,(R,K,s)          -- S(KS)K
           ]
overloadB' s = overloading s $ initB' s


-- (C)ardinal: flip    S(BBS)(KK)
initC',overloadC' :: SlotNum -> [Action]
initC' sn = mocking _fun ++ setArg _B ++ setRes ++ doApply _res -- s[_res] = BB
         ++ [(R,S,_res)                                        -- s[_res] = BBS
            ,(L,S,_res)                                        -- s[_res] = S(BBS)
            ,(L,Put,_arg)
            ,(R,K,_arg),(R,K,_arg)                             -- s[_arg] = (KK)
            ]
         ++ setFun _res
         ++ setRes
         ++ doApply _res
         ++ copy' _res sn
overloadC' sn = overloading sn $ initC' sn

initC,overloadC :: [Action]
initC = initC' _C
overloadC = overloading _C $ initC

-- (L)ark CBM
initY', overloadY' :: SlotNum -> [Action]
initY,overloadY :: [Action]

initL' sn = setArg _B ++ setFun _C ++ setRes ++ doApply _res -- s[_res] = CB
         ++ mocking' _arg                                   -- s[_arg] = M
         ++ setFun   _res                                   -- s[_fun] = CB
         ++ doApply  _res                                   -- s[_res] = CBM
         ++ copy' _res sn
overloadL' sn = overloading sn $ initL' sn

initY' sn = initL' _res ++ setFun _res ++ [(L,S,_fun)] -- s[_fun] = SL
          ++ setArg _res                               -- s[_arg] = L
          ++ setRes ++ doApply _res                    -- s[_res] = SLL
          ++ copy' _res sn
overloadY' sn = overloading sn $ initY' sn
initY = initY' _Y
overloadY = overloading _Y $ initY

-- ---------------------------------------------------------------------------

makeValue :: Value -> SlotNum -> [Action]
makeValue m s = execM (makeValue' m s) (IS.fromList [0,1])
{-
とりあえず、0,1以外は好き勝手破壊して良いことに
他に保護したい場所は [0,1] のところを書き換えれば良い
-}

type M = StateT IS.IntSet (Writer [Action])

allocSlot :: M SlotNum
allocSlot = do
  s <- get
  case filter (\i -> i `IS.notMember` s) [0..255] of
    [] -> error "no free slot"
    i:_ -> do
      put (IS.insert i s)
      return i      

freeSlot :: SlotNum -> M ()
freeSlot i = modify (IS.delete i)

execM :: M a -> IS.IntSet -> [Action]
execM m s = execWriter $ runStateT m s

makeValue' :: Value -> SlotNum -> M ()
makeValue' (IntVal n) dst   = tell $ makeNum' n dst
makeValue' (PAp c args) dst = f c (reverse args) dst
  where
    f :: Card -> [Value] -> SlotNum -> M ()
    f c [] dst = tell [(L,Put,dst), (R,c,dst)]
    f c (x:xs) dst = do
      tmp1 <- allocSlot
      tmp2 <- allocSlot
      f c xs tmp1
      makeValue' x tmp2
      tell $ copy' tmp1 1
      freeSlot tmp1
      tell $ copy' tmp2 0
      freeSlot tmp2
      tell $ apply' dst
      tell $ doApply dst
      return ()

-- ---------------------------------------------------------------------------
