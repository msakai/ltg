module SamplePlayer1 (samplePlayer1) where

import Control.Monad.State
import qualified Data.IntMap as IM
import Data.Maybe
import LTG
import Player
import Play (match)

{-
相手の生きているスロットに対してひたすら dec するだけのプレイヤー
-}

samplePlayer1 :: Player
samplePlayer1 = Player samplePlayer1'

samplePlayer1' :: GameState -> (Action, Player)
samplePlayer1' (p0,p1) =
  case tryToMakeN (255-target) p0 of
    Left action -> ( action, samplePlayer1 )
    Right i -> ( (L, Dec, i), samplePlayer1 )
  where
    target = findAliveTarget p1

findAliveTarget :: PlayerState -> Int
findAliveTarget (f,v) = head [i | i <- [255,254..0], alive (v IM.! i)]

findValue :: Value -> PlayerState -> Maybe Int
findValue val (f,v) = listToMaybe [i | (i, val') <- IM.toList f, val==val', alive (v IM.! i)]

tryToMakeN :: Int -> PlayerState -> Either Action SlotNum
tryToMakeN n p0 =
  case findValue (IntVal n) p0 of
    Just i -> Right i
    Nothing ->
      if n > 0
      then
        case tryToMakeN (n-1) p0 of
          Left action -> Left action
          Right j -> Left (L, Succ, j)
      else
        case findValue vI p0 of
          Just idSlot -> Left (R, Zero, idSlot)
          Nothing -> Left (L, Put, 0)

testSession :: IO ()
testSession = match samplePlayer1 samplePlayer1


