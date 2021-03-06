-- 思いつきで実装してみた関数などの置き場
--
-- 適当な名前を思いつかなくて長い名前になっているものもあります……。
-- 利用する際に長い名前が邪魔だと思ったら、適当に名前を変えてみて下さい。
module Misc where
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import LTG

reverseSlot :: SlotNum -> SlotNum
reverseSlot slot = 255 - slot

-- プレイヤーの i 番目と 255-i 番目の slot を見る
checkSlotState :: PlayerState -> SlotNum -> ((Value, Int), (Value, Int))
checkSlotState (fstSt,sndSt) slot
  = ((getVal    fstSt slot, getVal    sndSt slot)
    ,(getRevVal fstSt slot, getRevVal sndSt slot))


-- 自分の i 番目の slot と、相手の 255-i 番目の slot を見る
-- (GameStateが(自分,相手)の順番に実装されていることに依存)
checkSlotNum'sAccecibleSlotState :: GameState -> SlotNum -> ((Value, Int), (Value, Int))
checkSlotNum'sAccecibleSlotState ((p0fstSt,p0sndSt), (p1fstSt,p1sndSt)) slot
  = ((getVal    p0fstSt slot, getVal    p0sndSt slot)
    ,(getRevVal p1fstSt slot, getRevVal p1sndSt slot))


getVal, getRevVal :: IntMap a -> SlotNum -> a
getVal    state = (state IM.!)
getRevVal state = (state IM.!) . reverseSlot

valueLength :: Value -> Int
valueLength (IntVal _) = 1
valueLength (PAp c []) = 1
valueLength (PAp c args) = 1 + sum (map valueLength args)
