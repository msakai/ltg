{-
ランダムな手を繰り返すプレイヤー
-}
module Main where

import Control.Monad
import System.Environment (getArgs)
import System.Random
import LTG

main :: IO ()
main =   do
  args <- getArgs
  let isPlayer0 = (args /= ["1"])
  when isPlayer0 m
  forever $ do
    _ <- readAction
    m

m :: IO ()
m = do
  b <- randomIO
  let lr = if b then L else R
  card <- liftM toEnum $ randomRIO (fromEnum (minBound :: Card), fromEnum (maxBound :: Card))
  slot <- randomRIO (0,255)
  writeAction (lr, card, slot)

