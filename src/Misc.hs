-- 思いつきで実装してみた関数などの置き場
module Misc where
import qualified Data.IntMap as IM
import LTG

reverseSlot :: SlotNum -> SlotNum
reverseSlot slot = 255 - slot

-- 自分のi 番目と 255-i 番目の slot を見る
-- XXX 自分の i 番目の slot と、敵の 255-i 番目の slot を見る方が便利？
checkSlotState :: PlayerState -> SlotNum -> ((Value, Int), (Value, Int))
checkSlotState (fstSt,sndSt) slot
  = ((getVal    fstSt slot, getVal    sndSt slot)
    ,(getRevVal fstSt slot, getRevVal sndSt slot))
  where getVal    state = (state IM.!)
        getRevVal state = (state IM.!) . reverseSlot
