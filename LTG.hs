module LTG where

import qualified Data.IntMap as IM
import qualified Data.Map as Map

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
  deriving (Eq, Ord, Enum,Show,Read)

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

-- ---------------------------------------------------------------------------

-- (field, vitality)
type PlayerState = (IM.IntMap Value, IM.IntMap Int)

initialPlayerState :: PlayerState
initialPlayerState =
  ( IM.fromList [(i, PAp I []) | i <- [0..255]]
  , IM.fromList [(i, 10000) | i <- [0..255]]
  )

type GameState = (PlayerState, PlayerState)

initialState :: GameState
initialState = (initialPlayerState, initialPlayerState)

-- ---------------------------------------------------------------------------

data LR = L | R deriving (Ord, Eq, Show, Enum, Bounded)
type Action = (LR, Card, SlotNum)

