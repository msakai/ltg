module TaskLib where

{-
TODO:
* スロットが死んでるときの処理とかを考える
  TaskモナドをMonadErrorにして対処?
* スロット=レジスタの割り当て管理はどのレイヤでやる?
-}

import Control.Monad
import qualified Data.IntMap as IM
import Player

-- スロットlocにIをセットする
-- 他のスロットの値を参照したり壊したりしない
setI :: SlotNum -> Task ()
setI loc = do
  ((f,_),_) <- getState  
  unless ((f IM.! loc) == vI) $ execAction (L, Put, loc)

-- スロットlocにカードcをセットする
-- 他のスロットの値を参照したり壊したりしない
setCard :: Card -> SlotNum -> Task ()
setCard I loc = setI loc
setCard c loc = do
  ((f,_),_) <- getState  
  unless ((f IM.! loc) == cardValue c) $ do
    setI loc
    execAction (R, c, loc)

{-
スロットlocに値iを作成
他のスロットの値を参照したり壊したりしない

TODO: ターン数の最適化
-}
makeNum :: Int -> SlotNum -> Task ()
makeNum i loc = loop
  where
    loop = do
      ((f,_),_) <- getState
      case f IM.! loc of
        IntVal val
          | val == i ->
              return ()
          | val /= 0 && val*2 <= i -> do
              execAction (L, Dbl, loc)
              loop
          | val < i -> do
              execAction (L, Succ, loc)
              loop
        val ->
          if val == vI
          then do
            execAction (R, Zero, loc)
            loop
          else do
            execAction (L, Put, loc)
            loop

-- fromからtoにスロットの値をコピー
-- 他のスロットの値を参照したり壊したりしない
copySlot :: SlotNum -> SlotNum -> Task ()
copySlot from to = do
  makeNum from to
  execAction (L, Get, to)

-- スロット1の関数をスロット0の値に適用し、結果をresに格納する
apply1To0 :: SlotNum -> Task ()
apply1To0 res = applyNTo0 1 res

-- スロットfunの関数をスロット0の値に適用し、結果をresに格納する
applyNTo0 :: SlotNum -> SlotNum -> Task ()
applyNTo0 fun res = do
  setCard Get res
  replicateM_ fun $ mapM_ execAction $ 
    [(L,K,res), (L,S,res), (R,Succ,res)]
  mapM_ execAction $
    [(L,S,res), (R,Get,res)]
  execAction (R,Zero,res)
