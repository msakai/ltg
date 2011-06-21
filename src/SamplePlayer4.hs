module SamplePlayer4 (samplePlayer4) where

import Control.Monad.State
import qualified Data.IntMap as IM
import LTG
import Player
import Play
import TaskLib
import Lambda
import Debug.Trace

samplePlayer4 :: Player
samplePlayer4 = runTask' $ do
  {-
    無限ループでスロット0のvitalityを0xffffまで増やす
    λx → do
      help(0, 0, 8192)
      get(1)(x)
  -}
  let foo = comp1 (PAp S [PAp K [PAp Help [IntVal 0, IntVal 0]], PAp K [IntVal 8192]])
                  (PAp S [PAp S [PAp K [PAp Get []], PAp Succ []], PAp I []])
  makeValue foo 1 [2..]
  execAction (R, Zero, 1)  

  makeValue weapon 1 [2..]  
  forever loop  

loop :: Task ()
loop = do
  (_,p1) <- getState
  let target = findAliveTarget p1  
  makeNum (255 - target) 0
  execAction (R, Zero, weaponLoc)

findAliveTarget :: PlayerState -> Int
findAliveTarget (_,v) = head [i | i <- [255,254..0], alive (v IM.! i)]

{-
λx → do
  help(0, 0, 32768)
  attack(0, get(x), 2048)
  get(x+1)
引数には0を渡すこと前提
-}
weapon :: Value
weapon = comp1 act1 (comp1 act2 act3)
  where
    act1, act2, act3 :: Value
    act1 = PAp S [PAp K [PAp Help [IntVal 0, IntVal 0]], PAp K [IntVal 32768]]
    act2 = PAp S [PAp S [PAp K [PAp Attack [IntVal 0]], PAp Get []], PAp K [IntVal 2048]]
    act3 = PAp S [PAp K [PAp Get []], PAp Succ []]

weaponLoc :: SlotNum
weaponLoc = 1

comp1 :: Value -> Value -> Value
comp1 f g = PAp S [PAp S [PAp K [PAp Put []], f], g]

comp1' :: SlotNum -> SlotNum -> SlotNum -> [SlotNum] -> Task ()
comp1' f g dst (tmp1:tmp2:tmps) = do
  makeValue (PAp S [PAp K [PAp Put []]]) tmp1 (tmp2:tmps)
  copySlot f 0
  applyNTo0 tmp1 tmp2
  execAction (L, S, tmp2)
  copySlot g 0
  applyNTo0 tmp2 dst

testSession = only samplePlayer4
