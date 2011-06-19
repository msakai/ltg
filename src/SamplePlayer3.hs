{-
「スロット0の値iに対して、strength + 1 回 dec i する」処理を weaponLoc に作成し、
それを呼び出すのを繰り返す。
-}
module SamplePlayer3 (samplePlayer3) where

import Control.Monad.State
import qualified Data.IntMap as IM
import LTG
import Player
import Play
import TaskLib

samplePlayer3 :: Player
samplePlayer3 = runTask' $ do
  persist mkWeapon0 weaponLoc
  forever loop

weaponLoc :: SlotNum
weaponLoc = 1

tmpLoc1, tmpLoc2, tmpLoc3 :: SlotNum
tmpLoc1 = 3
tmpLoc2 = 4
tmpLoc3 = 5

strength :: Int
strength = 329
-- strength = 330 -- 1000回を超えてしまってng

persist :: Task () -> SlotNum -> Task ()
persist mkMain0 loc = do
  setCard K tmpLoc2
  execAction (L, K, tmpLoc2)
  execAction (L, S, tmpLoc2)
  -- f[tmpLoc2] = S(KK)

  setCard Get tmpLoc3
  execAction (L, K, tmpLoc3)
  execAction (L, S, tmpLoc3)
  -- f[tmpLoc2] = S(KK)
  -- f[tmpLoc3] = S(Kget)
  makeNum weaponLoc 0
  execAction (L, K, 0)
  -- f[tmpLoc2] = S(KK)
  -- f[tmpLoc3] = S(Kget)
  -- f[0] = Kn
  applyNTo0 tmpLoc3 tmpLoc1
  -- f[tmpLoc2] = S(KK)
  -- f[tmpLoc1] = S(Kget)(Kn)
  copySlot tmpLoc1 0
  -- f[tmpLoc2] = S(KK)
  -- f[0] = S(Kget)(Kn)
  applyNTo0 tmpLoc2 tmpLoc1
  -- f[tmpLoc1] = S(KK)(S(Kget)(Kn))
  execAction (L, S, tmpLoc1)
  -- f[tmpLoc1] = S(S(KK)(S(Kget)(Kn)))

  mkMain0
  -- f[0] = α
  -- f[tmpLoc1] = S(S(KK)(S(Kget)(Kn)))

  applyNTo0 tmpLoc1 loc
  -- f[loc] = S(S(KK)(S(Kget)(Kn)))α

mkWeapon0 :: Task ()
mkWeapon0 = do
  setCard Dec 0
  replicateM_ strength $ do
    execAction (L, S, 0)
    execAction (R, Dec, 0)
  execAction (L, K, 0)
  execAction (L, S, 0)
  execAction (R, Get, 0)

loop :: Task ()
loop = do
  (_,p1) <- getState
  let target = findAliveTarget p1  
  makeNum (255 - target) 0
  execAction (R, Zero, weaponLoc)

findAliveTarget :: PlayerState -> Int
findAliveTarget (_,v) = head [i | i <- [255,254..0], alive (v IM.! i)]

testSession :: IO ()
testSession = only samplePlayer3

