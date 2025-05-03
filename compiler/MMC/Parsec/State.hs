module MMC.Parsec.State where

data State s e = State
  { stateInput :: s,
    stateOffset :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)