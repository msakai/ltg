{-
「スロット0の値iに対して、strength + 1 回 dec i する」処理を weaponLoc に作成し、
それを callLoc にコピーしてから呼び出すというのを繰り返す。
-}
module SamplePlayer2 (samplePlayer2) where

import Control.Monad.State
import qualified Data.IntMap as IM
import Data.Maybe
import LTG
import Player
import Play

import SamplePlayer1 (samplePlayer1)

samplePlayer2 :: Player
samplePlayer2 = setup mkWeapon loop

setup :: [Action] -> Player -> Player
setup [] p = p
setup (x:xs) p = Player (\_ -> (x, setup xs p))

weaponLoc :: SlotNum
weaponLoc = 1

callLoc :: SlotNum
callLoc = 2

strength :: Int
strength = 330

mkWeapon :: [Action]
mkWeapon =
  [ (R, Dec, weaponLoc) ] ++
  concat (replicate strength [(L, S, weaponLoc), (R, Dec, weaponLoc)]) ++
  [ (L, K, weaponLoc)
  , (L, S, weaponLoc)
  , (R, Get, weaponLoc)
  ]

loop :: Player
loop = Player loop'

loop' :: GameState -> (Action, Player)
loop' (p0,p1) =
  case tryToMakeN (255 - target) p0 of
    Left action -> ( action, loop )
    Right i ->
      if fst p0 IM.! callLoc == fst p0 IM.! weaponLoc
      then ( (R, Zero, callLoc), loop )
      else
        case fst p0 IM.! callLoc of
          PAp I [] -> ( (R, Zero, callLoc), loop )
          IntVal v
            | v<weaponLoc  -> ( (L, Succ, callLoc), loop )
            | v==weaponLoc -> ( (L, Get, callLoc), loop )
          _  -> ( (L, Put, callLoc), loop )
  where
    target = findAliveTarget p1

findAliveTarget :: PlayerState -> Int
findAliveTarget (f,v) = head [i | i <- [255,254..0], alive (v IM.! i)]

findValue :: Value -> PlayerState -> Maybe Int
findValue val (f,v) = listToMaybe [i | (i, val') <- IM.toList f, val==val', alive (v IM.! i)]

tryToMakeN :: Int -> PlayerState -> Either Action SlotNum
tryToMakeN n p0 =
  case fst p0 IM.! 0 of
    IntVal m
      | m == n -> Right 0
      | m < n  -> Left (L, Succ, 0)
      | otherwise -> Left (L, Put, 0)
    PAp I [] -> Left (R, Zero, 0)

testSession :: IO ()
testSession = do
  runStateT (play samplePlayer2 samplePlayer1) initialState
  return ()
