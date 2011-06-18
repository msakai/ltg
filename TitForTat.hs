{-
相手の手をそのまま繰り返すだけのプレイヤープログラム
-}
module Main where

import Control.Monad
import System.Environment (getArgs)
import LTG

main :: IO ()
main =   do
  args <- getArgs
  let isPlayer0 = (args /= ["1"])
  when isPlayer0 $ writeAction (L, Put, 0)
  forever $ do
    action <- readAction
    writeAction action
