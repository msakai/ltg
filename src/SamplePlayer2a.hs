{-
「スロット0の値iに対して、strength + 1 回 dec i する」処理を weaponLoc に作成し、
それを callLoc にコピーしてから呼び出すというのを繰り返す。

SamplePlayer2.hs をTaskモナドで書きなおした物
-}
module SamplePlayer2a (samplePlayer2) where

import Control.Monad.State
import qualified Data.IntMap as IM
import LTG
import Player
import Play
import TaskLib

import SamplePlayer1 (samplePlayer1)

samplePlayer2 :: Player
samplePlayer2 = runTask' $ do
  mkWeapon
  forever loop

weaponLoc :: SlotNum
weaponLoc = 1

callLoc :: SlotNum
callLoc = 2

strength :: Int
strength = 330

mkWeapon :: Task ()
mkWeapon = do
  setCard Dec weaponLoc
  replicateM_ strength $ do
    execAction (L, S, weaponLoc)
    execAction (R, Dec, weaponLoc)
  execAction (L, K, weaponLoc)
  execAction (L, S, weaponLoc)
  execAction (R, Get, weaponLoc)

loop :: Task ()
loop = do
  (_,p1) <- getState
  let target = findAliveTarget p1  
  makeNum (255 - target) 0
  copySlot weaponLoc callLoc
  execAction (R, Zero, callLoc)

findAliveTarget :: PlayerState -> Int
findAliveTarget (_,v) = head [i | i <- [255,254..0], alive (v IM.! i)]

testSession :: IO ()
testSession = do
  _ <- runStateT (play samplePlayer2 samplePlayer1) initialState
  return ()
