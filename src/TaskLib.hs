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
setI :: SlotNum -> Task ()
setI loc = do
  ((f,v),_) <- getState  
  when ((f IM.! loc) /= vI) $ execAction (L, Put, loc)

-- スロットlocにカードcをセットする
setCard :: SlotNum -> Card -> Task ()
setCard loc I = setI loc
setCard loc c = do
  ((f,v),_) <- getState  
  unless ((f IM.! loc) == PAp c []) $ do
    setI loc
    execAction (R, c, loc)

{-
スロットlocに値iを作成
他のスロットの値を参照したり壊したりしない

TODO:
* ターン数の最適化
-}
makeNum :: Int -> SlotNum -> Task ()
makeNum i loc = loop
  where
    loop = do
      ((f,v),_) <- getState
      let val = f IM.! loc
      case val of
        IntVal val
          | val == i ->
              return ()
          | val*2 <= i -> do
              execAction (L, Dbl, loc)
              loop
          | val < i -> do
              execAction (L, Succ, loc)
              loop
        _ ->
          if val == vI
          then do
            execAction (R, Zero, loc)
            loop
          else do
            execAction (L, Put, loc)
            loop

-- fromからtoにスロットの値をコピー
copySlot :: SlotNum -> SlotNum -> Task ()
copySlot from to = do
  makeNum from to
  execAction (L, Get, to)

-- スロット1の関数をスロット0の値に適用し、結果をresに格納する
apply1To0 :: SlotNum -> Task ()
apply1To0 res = do
  setI res
  mapM_ execAction $
    [ (R,Get,res)
    , (L,K,res)
    , (L,S,res)
    , (R,Succ,res)
    , (L,S,res)
    , (R,Get,res)
    , (R,Zero,res)
    ]
  