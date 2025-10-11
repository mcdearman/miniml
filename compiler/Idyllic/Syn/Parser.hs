module Idyllic.Syn.Parser where

-- import Data.ByteString (ByteString)
-- import Data.Vector (MVector, Vector)
-- import MMC.Syn.GreenNode

-- data Parser = Parser
--   { src :: !ByteString,
--     tokens :: !(Vector Token),
--     fuel :: !Int,
--     events :: !(MVector Int Event)
--   }

-- data Event = Open !SyntaxKind | Close | Advance deriving (Show, Eq)

-- newtype MarkOpened = MarkOpened Int deriving (Show, Eq)

-- open :: Parser -> MarkOpened
-- open !p = undefined