module LTG where

import Control.Monad
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import qualified Data.Map as Map
import System.IO

-- ---------------------------------------------------------------------------

type SlotNum = Int

isValidSlotNum :: Int -> Bool
isValidSlotNum n = 0 <= n && n <= 255

data Value = IntVal Int | PAp Card [Value]
  deriving (Eq, Ord)

instance Show Value where
  show (IntVal n) = show n
  show (PAp c []) = show c
  show (PAp c args) = show c ++ concat (map f args)
    where
      f x = "(" ++ show x ++ ")"

dead, alive :: Int -> Bool
dead v = v == 0 || v == -1
alive = not . dead

data Card
  = I
  | Zero
  | Succ
  | Dbl
  | Get
  | Put
  | S
  | K
  | Inc
  | Dec
  | Attack
  | Help
  | Copy
  | Revive
  | Zombie
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

arity :: Card -> Int
arity I = 1
arity Zero = 0
arity Succ = 1
arity Dbl = 1
arity Get = 1
arity Put = 1
arity S = 3
arity K = 2
arity Inc = 1
arity Dec = 1
arity Attack = 3
arity Help = 3
arity Copy = 1
arity Revive = 1
arity Zombie = 2

cardName :: Card -> String
cardName I      = "I"
cardName Zero   = "zero"
cardName Succ   = "succ"
cardName Dbl    = "dbl"
cardName Get    = "get"
cardName Put    = "put"
cardName S      = "S"
cardName K      = "K"
cardName Inc    = "inc"
cardName Dec    = "dec"
cardName Attack = "attack"
cardName Help   = "help"
cardName Copy   = "copy"
cardName Revive = "revive"
cardName Zombie = "zombie"

cardOfName :: String -> Card
cardOfName = (m Map.!)
  where
    m = Map.fromList [(cardName card, card) | card <-[I .. Zombie]]

cardValue :: Card -> Value
cardValue Zero = IntVal 0
cardValue c = PAp c []

vI, vZero, vSucc, vDbl, vGet, vPut, vS, vK, vInc, vDec, vAttack, vHelp, vCopy, vRevive, vZombie :: Value
vI      = cardValue I
vZero   = cardValue Zero
vSucc   = cardValue Succ
vDbl    = cardValue Dbl
vGet    = cardValue Get
vPut    = cardValue Put
vS      = cardValue S
vK      = cardValue K
vInc    = cardValue Inc
vDec    = cardValue Dec
vAttack = cardValue Attack
vHelp   = cardValue Help
vCopy   = cardValue Copy
vRevive = cardValue Revive
vZombie = cardValue Zombie

-- 状態に依存しない純粋関数的なカード
pureCards :: [Card]
pureCards =
  [I, Zero, Succ, Dbl, Put, S, K]

-- 状態に依存するが状態を変更しないカード
rdCards :: [Card]
rdCards = [Get, Copy]

-- 状態に依存し、状態を変更するカード
wrCards :: [Card]
wrCards = [Inc, Dec, Attack, Help, Revive, Zombie]

-- ---------------------------------------------------------------------------

-- (field, vitality)
type PlayerState = (IM.IntMap Value, IM.IntMap Int)

initialPlayerState :: PlayerState
initialPlayerState =
  ( IM.fromList [(i, vI) | i <- [0..255]]
  , IM.fromList [(i, 10000) | i <- [0..255]]
  )

showPlayerState :: PlayerState -> String
showPlayerState = show . g
  where
    g (f,v) = [slot | i <- [0..255], let slot = (i, (v ! i, f ! i))
                    , f ! i /= vI || v ! i /= 10000]

type GameState = (PlayerState, PlayerState)

initialState :: GameState
initialState = (initialPlayerState, initialPlayerState)

printState :: GameState -> IO ()
printState = hPrintState stdout

hPrintState :: Handle -> GameState -> IO ()
hPrintState h (p1,p2) = do
  hPutStrLn h $ showPlayerState p1
  hPutStrLn h $ showPlayerState p2

-- ---------------------------------------------------------------------------

data LR = L | R deriving (Ord, Eq, Show, Enum, Bounded)
type Action = (LR, Card, SlotNum)

readAction :: IO Action
readAction = do
  lr <- readLn
  case (lr::Int) of
    1 -> do
      card <- liftM cardOfName getLine
      slot <- readLn
      return (L, card, slot)
    2 -> do
      slot <- readLn
      card <- liftM cardOfName getLine
      return (R, card, slot)

writeAction :: Action -> IO ()
writeAction (lr,card,slot) = do
  case lr of
    L -> do
      putStrLn "1"
      putStrLn (cardName card)
      print slot
    R -> do
      putStrLn "2"
      print slot
      putStrLn (cardName card)
  hFlush stdout
