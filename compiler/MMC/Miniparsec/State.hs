module MMC.Miniparsec.State (State (..)) where

data State s d e = State
  { stateInput :: s,
    stateOffset :: {-# UNPACK #-} !Int,
    stateMetadata :: d,
    stateErrors :: [e]
  }
  deriving (Eq, Show)